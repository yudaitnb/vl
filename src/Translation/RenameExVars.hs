module Translation.RenameExVars where

import Control.Monad.Trans.State

import Data.Map (Map, (!))
import qualified Data.Map as M

import Syntax.LambdaVL
import qualified Syntax.Name as N
import Syntax.SrcLoc (SrcSpanInfo(..))
import Syntax.Env
import Syntax.Type

import Util
import Control.Monad (forM_, forM)
import Data.List (nub)

-- exp中の外部モジュール由来の値を参照する変数(f)の複製に従い,
-- tenv, uenv, cs中の各fに関連する型制約, 型制約, 制約を複製する
-- e.g.) 
-- exp:  f 1 + f 2 -> main = f' 1 f'' 2
-- tenv: f: [!_a1 Int -> Int]_a2 -> f': [!_a1' Int -> Int]_a2', f'': [!_a1'' Int -> Int]_a2''
-- uenv: a1:Label, a2:Label -> a1':Label, a2':Label, a1'':Label, a2'':Label 
-- cs:   (a2 <= Av1 && a2 <= a2_v1 && a1 <= ??) || (a2 <= Av2 && a2 <= a2_v1 && a1 <= ??)
--    -> ((a2'  <= Av1 && a2'  <= a2_v1 && a1'  <= ??) || (a2'  <= Av2 && a2'  <= a2_v1 && a1'  <= ??)) &&
--       ((a2'' <= Av1 && a2'' <= a2_v1 && a1'' <= ??) || (a2'' <= Av2 && a2'' <= a2_v1 && a1'' <= ??))
-- 結局a2_v1/a2_v2のバージョンは1つに決まらなければならないが、これは言語設計の制約の一つなので問題ない

type CounterTable = Map String Int

data RenameEnv' = RenameEnv'
  { counterTable :: CounterTable  -- 各外部モジュール変数の累積数
  , boundVars :: [String] -- そのスコープにおける束縛変数のリスト
  } deriving (Show)
type RenameEnv a = State RenameEnv' a

mkInitRenameEnv :: [Decl SrcSpanInfo] -> RenameEnv'
mkInitRenameEnv decls = RenameEnv'
        (M.fromList $ zip (freeVarsDecls decls) (repeat 0)) $
        map (\(PatBind _ p@(PVar _ n) e) -> N.getName n) decls
          ++ ["+", "-", "*", "/", "&&", "||", "<" ,"<=", ">", ">=", "==", "/=" ]
  where
    freeVarsDecls :: [Decl SrcSpanInfo] -> [String]
    freeVarsDecls [] = []
    freeVarsDecls (d:rst) = (nub . freeVars . getBind $ d) ++ freeVarsDecls rst

getCounterOf :: String -> RenameEnv Int
getCounterOf s = do
  gets $ (! s) . counterTable

addCounterOf :: String -> RenameEnv ()
addCounterOf s = do
  oldct <- gets counterTable
  oldc <- getCounterOf s
  modify $ \env -> env { counterTable = M.insert s (oldc + 1) oldct }

newNameOfExVar :: String -> Int -> String
newNameOfExVar s c = "__" ++ s ++ "_" ++ show c

newNamesOfExVar :: String -> Int -> [String]
newNamesOfExVar s c = take c $ map (newNameOfExVar s) [0..]

newNameOfExTyVar :: String -> Int -> String
newNameOfExTyVar s c = s ++ "_" ++ show c

newNamesOfExTyVar :: String -> Int -> [String]
newNamesOfExTyVar s c = take c $ map (newNameOfExTyVar s) [0..]

genNewVarFrom :: Exp SrcSpanInfo -> RenameEnv (Exp SrcSpanInfo)
genNewVarFrom (Var l1 (N.UnQual l2 (N.Ident l3 s))) = do
  c <- getCounterOf s
  let newVarName = newNameOfExVar s c
      newVar = Var l1 (N.UnQual l2 (N.Ident l3 newVarName))
  addCounterOf s
  return newVar
genNewVarFrom e = error $ "The function genNewVarFrom is not defined for a given expression: " ++ putDocString (ppP e)
  
isFree :: Exp SrcSpanInfo -> RenameEnv Bool
isFree v@(Var _ _) = gets $ not . (N.getName v `elem`) . boundVars
isFree e           = error $ "The function genNewVarFrom is not defined for a given expression: " ++ putDocString (ppP e)

addBoundVar :: String -> RenameEnv ()
addBoundVar s = do
  bv <- gets boundVars
  modify $ \env -> env { boundVars = s : bv }

setBoundVar :: [String] -> RenameEnv ()
setBoundVar ss = modify $ \env -> env { boundVars = ss }

addBoundVarFromPat :: Pat SrcSpanInfo -> RenameEnv ()
addBoundVarFromPat p = case p of
  PVar _ name -> addBoundVar $ N.getName name
  PBox _ p'   -> addBoundVarFromPat p'
  PWildCard _ -> return ()
  PLit {}     -> return ()

----------------

renameExVarModule :: Module SrcSpanInfo -> (Module SrcSpanInfo, CounterTable)
renameExVarModule mod@(Module l mh imps decls) =
  let (decls', renv) = runState (forM decls renameExVarDecl) (mkInitRenameEnv decls)
  in (Module l mh imps decls', counterTable renv)

renameExVarDecl :: Decl SrcSpanInfo -> RenameEnv (Decl SrcSpanInfo)
renameExVarDecl pb@(PatBind l p@(PVar _ name) e) = PatBind l p <$> renameExVar e

renameExVar :: Exp SrcSpanInfo -> RenameEnv (Exp SrcSpanInfo)
renameExVar exp = case exp of
  Lit _ _ -> return exp
  Var _ _ -> isFree exp >>= \b -> if b
              then genNewVarFrom exp -- 自由変数は外部モジュール由来のはず
              else return exp        -- パターンかλに束縛された変数
  App l e1 e2   -> App l <$> renameExVar e1 <*> renameExVar e2
  -- If even one pattern that binds the same name exists, terminate the renameExVar function,
  Lambda l p e -> do
    oldBoundVars <- gets boundVars   -- 旧テーブルを保存
    addBoundVarFromPat p             -- 束縛変数から新しいテーブルを環境に登録/新しい束縛変数リストを得る
    e' <- renameExVar e              -- 更新された環境でrename
    setBoundVar oldBoundVars         -- 旧テーブルに戻す
    return $ Lambda l p e'           -- rename済み式と新しい束縛変数リストを用いてLambdaのrename済み式を構成
  If l e1 e2 e3  -> If l <$> renameExVar e1 <*> renameExVar e2 <*> renameExVar e3
  Pr l e         -> Pr l <$> renameExVar e
  VRes l label e -> VRes l label <$> renameExVar e
  VExt l e       -> VExt l <$> renameExVar e
  

-------------------

duplicateEnvs :: CounterTable -> (TEnv, UEnv, Constraints) -> (TEnv, UEnv, Constraints)
duplicateEnvs ct (tenv, uenv, cs) = duplicateEnvs' (M.toList ct) (tenv, uenv, cs)
  -- counetr : 型変数用の通し番号。uenvの複製に使う
  -- ct : 各外部モジュール変数がdecls内で総計何回複製されたか
  where
    -- (["f", 2]) ([f : [!_a1 Int -> Int]_a0], [a0:Labels,a1:Labels], [a0 <= C_a0, a1 <= C_a1])
    -- -> ( [__f_0 : [!_a1_0 Int -> Int]_a0_0, __f_1 : [!_a1_1 Int -> Int]_a0_1,]
    --    , [a0_0:Labels, a1_0:Labels, a0_1:Labels, a1_1:Labels]
    --    , [a0_0 <= C_a0, a1_0 <= C_a1, a0_1 <= C_a0, a1_1 <= C_a1])
    duplicateEnvs' :: [(String,Int)] -> (TEnv, UEnv, Constraints) -> (TEnv, UEnv, Constraints)
    duplicateEnvs' []          envs = envs
    duplicateEnvs' ((s,i):rst) (oldTEnv, oldUEnv, oldCs) = 
      let fvOfS = freeVars $ oldTEnv ! s
          newTEnv = duplicateVarsTEnv s i oldTEnv
          newUEnv = duplicateTyVarsUEnv fvOfS i oldUEnv
          newCs   = duplicateTyVarsCons fvOfS i oldCs
      in duplicateEnvs' rst (newTEnv, newUEnv, newCs)
    duplicateTyVarsUEnv :: [String] -> Int -> UEnv -> UEnv 
    duplicateTyVarsUEnv []       i uenv = uenv
    duplicateTyVarsUEnv (tyn:ss) i uenv = 
      let kind = uenv ! tyn
          uenvDeleted = M.delete tyn uenv
          newUEnv = foldl (\acc newtyn -> M.insert newtyn kind acc)
            uenvDeleted
            (newNamesOfExTyVar tyn i) -- ["a0_0", "a0_1"]
      in duplicateTyVarsUEnv ss i newUEnv
    duplicateTyVarsCons :: [String] -> Int -> Constraints -> Constraints
    duplicateTyVarsCons ss i cs = case cs of
      CTop          -> CTop
      CSubset t1 t2 -> case t1 of
        TyVar n ->
          let s = getName n in
          if s `elem` ss
            then foldl
              (\acc newtyn -> CSubset (TyVar $ Ident newtyn) t2 `landC` acc)
              CTop (newNamesOfExTyVar s i) -- ["a0_0", "a0_1"]
            else cs
        _       -> cs
      CAnd c1 c2    -> CAnd (duplicateTyVarsCons ss i c1) (duplicateTyVarsCons ss i c2)
      COr c1 c2     -> COr  (duplicateTyVarsCons ss i c1) (duplicateTyVarsCons ss i c2)
    duplicateVarsTEnv :: String -> Int -> TEnv -> TEnv
    duplicateVarsTEnv s i tenv =
      let envty = tenv ! s -- [!_a1 Int -> Int]_a0]
          tenvDeleted = M.delete s tenv
          newTEnv = foldl (\acc (i, newvn) -> M.insert newvn (renameEnvTyWithIdx i envty) acc)
              tenvDeleted $
              zip [0..] (newNamesOfExVar s i) -- [(0, "__f_0"), (1, "__f_1")]
      in newTEnv
      where
        renameEnvTyWithIdx :: Int -> EnvType -> EnvType
        renameEnvTyWithIdx i et = case et of
          NType  tag t   -> NType tag (renameTyWithIdx i t)
          GrType tag t c -> GrType tag (renameTyWithIdx i t) (renameTyWithIdx i c)
        renameTyWithIdx :: Int -> Type -> Type
        renameTyWithIdx i ty = case ty of
          TyVar (Ident n) -> TyVar (Ident $ newNameOfExVar n i)
          TyCon qn -> ty
          TyFun t1 t2 -> TyFun (renameTyWithIdx i t1) (renameTyWithIdx i t2)
          TyBox c t -> TyBox (renameTyWithIdx i c) (renameTyWithIdx i t)
          TyBottom -> ty
          TyLabels _ -> ty
          -- CAnd c1 c2 -> CAnd (renameTyWithIdx i c1) (renameTyWithIdx i c2)
          -- COr c1 c2 -> COr (renameTyWithIdx i c1) (renameTyWithIdx i c2)