module Translation.Normalize where

import Language.Desugared
import Data.Map

import Control.Monad.State
import Util

class Normalize ast where
  normal :: ast -> NormalizeEnv ast

data NormalizeEnv' = NormalizeEnv'
  { counter :: Int             -- 束縛変数の累積数
  , usedName :: [VarName]       -- 使用済み変数名
  }
type NormalizeEnv a = State NormalizeEnv' a

genNewVar :: NormalizeEnv VarName
genNewVar = do
  c <- gets counter
  used <- gets usedName
  let nvtmp = prefixVar ++ show c
  nv <- if nvtmp `elem` used
    then genNewVar
    else return nvtmp
  modify (\x -> x { usedName = nv : used })
  return nv

prefixVar :: String
prefixVar = "_tmp_"

instance Normalize (Exp l) where
  normal exp = case exp of
    Var l qn        -> return $ Var l qn
    Lit l lit       -> return $ Lit l lit
    App l e1' e2    -> return $ App l e1' e2
    Lambda l ps e   -> return $ Lambda l ps e
    Tuple l elms    -> return $ Tuple l elms
    List l elms     -> return $ List l elms
    If l ec et ee   -> return $ If l ec et ee
    -- Let l p e1 e2   -> Let l p e1 e2
    VRes l vbs e    -> do
      str <- genNewVar
      let pat = PVar l (Ident l str)
          var = Var l (UnQual l (Ident l str))
          res = App l (Lambda l pat $ VRes l vbs var) e
      return res
    VExt l e        -> return $ VExt l e

instance Normalize (Module l) where
  normal (Module l moduleHead pragmas importDecl decl) = do
    decl' <- mapM normal decl
    return $ Module l moduleHead pragmas importDecl decl'

instance Normalize (Decl l) where
  normal pb@(PatBind srcLocInfo pat e) = do
    e' <- normal e
    return $ PatBind srcLocInfo pat e'

normalize :: Module l -> Module l
normalize m = evalState (normal m) (NormalizeEnv' 0 (vars m))
