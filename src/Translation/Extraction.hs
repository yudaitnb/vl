module Translation.Extraction where

import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.State

import Language.LambdaVL

import Translation.Girard ( girardBck )

import Syntax.Common
import Syntax.Type ( Type ) 

import Parser (VLMod(..))
import Inference.TypeInference (TypedExp)

import Util

import Language.Haskell.Exts.Pretty
import Compile (CompileEnv'(..))
import Solver.Engine.SolverZ3 (combineSolRes)

data Env' = Env'
  { table :: [String] -- 束縛変数を記録する環境。スコープ生成の度に変わり得る。
  , context :: VLMod -- 今どのモジュールのどのバージョンの定義をコンパイルしているか。同モジュール内別定義参照のコンパイルに使う。
  , exVars :: Map VarKey (VarKey, VLMod, Type, Label) -- 全ての外部変数と、solverの計算した適切なラベルの対応
  , vlDecls :: Map VLMod (Map VarName (Exp SrcSpanInfo)) -- 全てのトップレベル宣言
  }
type Env a = State Env' a

extract env logFilePath solResMap = do
  logP "\n=== Labels of External Variables ==="
  let exVarsRes = duplicatedExVars env -- 後で消したい(envの中に入れる？)
  let exVarLabels = combineSolRes exVarsRes solResMap
  logPD $ ppP exVarLabels

  logP "\n=== VLDecls ==="
  let vldecls = mapVLDecls env
  logP vldecls

  let primEnv = Env' [] (VLMod "Main" Root) exVarLabels vldecls

  logP "\n=== Extraction ==="
  let expMain = vldecls <!> VLMod "Main" Root <!> "main"
  let extracted = evalState (extract' expMain) primEnv
      hsCodeBundled = girardBck extracted
  logP $ prettyPrint hsCodeBundled

  return hsCodeBundled
  where
    logP :: PrettyAST a => a -> IO ()
    logP = logPpLn ppP logFilePath
    logPD :: Doc ann -> IO ()
    logPD = logPpLnDoc logFilePath

extract' :: Exp SrcSpanInfo -> Env (Exp SrcSpanInfo)
extract' e = do
  exVarLabels <- gets exVars 
  vldecls <- gets vlDecls
  ctx <- gets context
  case e of
    Var l qn ->
      case M.lookup (mkVKFromQN qn) exVarLabels of
        -- exVarLabelsに含まれない -> 外部変数ではない
        -- 再帰出現している -> UnQualifyする
        -- 再帰出現していない -> 
        --   ローカル変数　-> なにもしない
        --   そのモジュールの別の関数 -> CLetで新しいスコープを作成し、そこに定義を代入する
        Nothing  -> do
          let qn' = case qn of
                  Qual l' _ n -> UnQual l' n
                  _           -> qn
          return $ Var l qn'
        -- exVarLabelsに含まれる -> 外部変数
        -- CLetで新しいスコープを作成し、そこに定義を代入する
        -- __f_2 -> let [f] = (substituted def) in f
        Just (orig, vlmod, _, label) -> do
          let definedMn = case getMN orig of
                  Nothing -> error  "Is orig exvar, right?"
                  Just mn -> mn
              vers = label <!> definedMn
              target = VLMod definedMn (head vers)
              subst = fromMaybe
                      (error $ putDocString $
                        line <> ppP orig <+> ppP "is not included in vldecls <!> target:\n" <>
                        ppP (vldecls <!> target)) $
                      M.lookup (getVN orig) (vldecls <!> target)
              origid = Ident l $ getVN orig
              p = PBox l $ PVar l origid
              e = Var l $ UnQual l origid
          CLet l p <$> (Pr l <$> extract' subst) <*> return e
            -- App l
            --   (Lambda l p e)
            --   (Pr l (extractExp subst))
    Con l qn        -> return e
    Lit {}          -> return e
    App l e1 e2     -> App l <$> extract' e1 <*> extract' e2
    CLet l p e1 e2  -> CLet l p <$> extract' e1 <*> extract' e2
    Lambda l p e    -> Lambda l p <$> extract' e
    Tuple l elms    -> Tuple l <$> mapM extract' elms
    List l elms     -> List l <$> mapM extract' elms
    Case l e alts   -> Case l e <$> mapM (\(Alt l' p' e') -> Alt l' p' <$> extract' e') alts
    If l e1 e2 e3   -> If l <$> extract' e1 <*> extract' e2 <*> extract' e3
    Pr l e          -> Pr l <$> extract' e
    VRes l lbl e    -> VRes l lbl <$> extract' e
    VExt l e        -> VExt l <$> extract' e
