module Translation.RenameExVars where

import Control.Monad.Trans.State

import Data.Map (Map, (!))
import qualified Data.Map as M

import Syntax.LambdaVL
import Syntax.Name as N

import Util
import Control.Monad (forM_)
import Data.List (nub)
import Syntax.SrcLoc (SrcSpanInfo(..))

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

data RenameEnv' = RenameEnv'
  { counterTable :: Map String Int  -- 各外部モジュール変数の累積数
  , boundVars :: [String] -- そのスコープにおける束縛変数のリスト
  } deriving (Show)
type RenameEnv a = State RenameEnv' a

getCounterOf :: String -> RenameEnv Int
getCounterOf s = do
  gets $ (! s) . counterTable

addCounterOf :: String -> RenameEnv ()
addCounterOf s = do
  oldct <- gets counterTable
  oldc <- getCounterOf s
  modify $ \env -> env { counterTable = M.insert s (oldc + 1) oldct }

genNewVarFrom :: Exp SrcSpanInfo -> RenameEnv (Exp SrcSpanInfo)
genNewVarFrom (Var l1 (N.UnQual l2 (N.Ident l3 s))) = do
  c <- getCounterOf s
  let newVarName = "__" ++ s ++ "_" ++ show c
      newVar = Var l1 (N.UnQual l2 (N.Ident l3 newVarName))
  addCounterOf s
  return newVar
genNewVarFrom e = error $ "The function genNewVarFrom is not defined for a given expression: " ++ putDocString (ppP e)
  
isFree :: Exp SrcSpanInfo -> RenameEnv Bool
isFree v@(Var _ _) = gets $ not . (getName v `elem`) . boundVars
isFree e           = error $ "The function genNewVarFrom is not defined for a given expression: " ++ putDocString (ppP e)

addBoundVar :: String -> RenameEnv ()
addBoundVar s = do
  bv <- gets boundVars
  modify $ \env -> env { boundVars = s : bv }

setBoundVar :: [String] -> RenameEnv ()
setBoundVar ss = modify $ \env -> env { boundVars = ss }

addBoundVarFromPat :: Pat SrcSpanInfo -> RenameEnv ()
addBoundVarFromPat p = case p of
  PVar _ name -> addBoundVar $ getName name
  PBox _ p'   -> addBoundVarFromPat p'
  PWildCard _ -> return ()
  PLit {}     -> return ()

----------------

mkRenameEnv :: Decl SrcSpanInfo -> RenameEnv'
mkRenameEnv (PatBind l p@(PVar _ name) e) = RenameEnv'
        (M.fromList $ zip (nub $ freeVars e) (repeat 0))
        [getName name, "+", "-", "*", "/", "&&", "||", "<" ,"<=", ">", ">=", "==", "/=" ]

renameExVarDecl :: Decl SrcSpanInfo -> (Decl SrcSpanInfo, RenameEnv')
renameExVarDecl pb@(PatBind l p@(PVar _ name) e) = 
  let initEnv = mkRenameEnv pb -- 再帰関数のため
  in runState (PatBind l p <$> renameExVar e) initEnv

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
  