{-# LANGUAGE TypeFamilies #-}
module TypeInference where

import qualified Syntax.LambdaVL as VL
import Data.Maybe (fromMaybe)
import Control.Monad.State
import Data.Map

import Syntax.Type
import Syntax.Kind
import Syntax.Env
import Syntax.Substitution
import TypeUnification
import Kinding
import Prelude hiding (lookup)
import TypeSubstitution

-- inferType :: Typable ast => ast -> (ast, SubstMap)
-- inferType ast = evalState (infer ast) primEnv

data Env' = Env'
  { counter :: Int -- 単一化型変数の累積数
  , tEnv :: TEnv   -- 型付け環境 / [x:A]
  , uEnv :: UEnv   -- 単一化用型変数環境 / [X:κ]
  }
type Env a = State Env' a

primEnv :: Env'
primEnv = Env' 0 empty empty

prefixNewTyVar :: String
prefixNewTyVar = "_tyvar_"

getTEnv :: Env TEnv
getTEnv = state $ \env@(Env' _ t _) -> (t, env)

getUEnv :: Env UEnv
getUEnv = state $ \env@(Env' _ _ u) -> (u, env)

setTEnv :: TEnv -> Env ()
setTEnv tenv = state $ \(Env' c _ u) -> ((), Env' c tenv u)

setUEnv :: UEnv -> Env ()
setUEnv uenv = state $ \(Env' c t _) -> ((), Env' c t uenv)

addTEnv :: String -> EnvType -> Env ()
addTEnv str envType = state $ \(Env' c t u) ->
  let tenv' = insert str envType t in
  return $ Env' c tenv' u

addUEnv :: String -> Kind -> Env ()
addUEnv str kind = state $ \(Env' c t u) ->
  let uenv' = insert str kind u in
  return $ Env' c t uenv'

genNewTyVar :: Kind -> Env ()
genNewTyVar kind = state $ \(Env' c t u) ->
  let strvar' = prefixNewTyVar ++ show c
      counter' = c + 1
      uenv' = insert strvar' kind u
  in return $ Env' counter' t uenv'

class Typable ast where
  infer :: ast -> Env (Type, SubstMap)

instance Typable (VL.Exp l) where
  infer exp = case exp of
    -- ^ ⇒_int
    VL.Lit _ (VL.Char {})   -> return (TyCon (UnQual (Ident "Char")), empty)
    VL.Lit _ (VL.String {}) -> return (TyCon (UnQual (Ident "String")), empty)
    VL.Lit _ (VL.Int {})    -> return (TyCon (UnQual (Ident "Int")), empty)
    -- ^ ⇒_Lin, ⇒_Gr
    VL.Var _ qName -> do
      tenv_old <- getTEnv
      let str = VL.getName qName
      case lookup str tenv_old of
        Nothing  -> error $ "[Exp - infer@TypeInference.hs] The variable " ++ str ++ " is not in the typing context."
        Just res -> case res of
          NType  ty   -> return (ty, empty)
          GrType ty c -> do
            uenv <- getUEnv
            isLabelsKind uenv c
            return (TyBox c ty, empty)
    -- ^ ⇒_app
    VL.App _ e1 e2 -> do
      (funTy, theta_1) <- infer e1
      delta_1 <- getTEnv
      (tya', theta_2) <- infer e2
      case funTy of
        TyFun tya tyb -> do
          uenv <- getUEnv
          let theta_3 = unify uenv tya tya'
              theta_4 = union theta_1 theta_2 `union` theta_3
              tyb' = typeSubstitution theta_4 tyb
          delta_2 <- getTEnv
          setTEnv $ delta_1 .++ delta_2
          return (tyb', theta_4)
        _ -> error $ "[Exp - infer@TypeInference.hs] The variable " ++ show funTy ++ " is not a function type."
    -- ^ ⇒_pr
    VL.Pr _ exp -> do
      error "" -- [TODO] 文脈変換関数が必要
    -- ^ ⇒_abs
    VL.Lambda _ pats exp -> error "[Exp - infer@TypeInference.hs] The function `infer` is not defined for a 'Lambda'."
    _              -> error "[Exp - infer@TypeInference.hs] The function `infer` is not defined for a given expression."

-- instance Typable (Module) where
--   infer (Module moduleHead modulePragma importDecl decl) = do decl' <- mapM infer decl
--                                                                  return $ Module moduleHead modulePragma importDecl decl'
--   infer _ = error "[Module - infer@infer.hs] The alpha-inferExp function is not defined for a given expression."

-- instance Typable (Decl) where
--   infer (FunBind match) = do ms' <- mapM infer match
--                                 return $ FunBind ms'
--   infer (PatBind pat rhs maybeBinds) = do oldTable <- getTable
--                                              rhs' <- infer rhs
--                                              setTable oldTable
--                                              return $ PatBind pat rhs' maybeBinds -- TODO:bindsも同様にスコープを作る。
--   infer _ = error "[Decl - infer@infer.hs] The alpha-inferExp function is not defined for a given expression."

-- instance Typable (Match) where
--   infer (Match name ps rhs maybeBinds) = do oldTable <- getTable
--                                                ps' <- genNewVarPats ps
--                                                rhs' <- infer rhs
--                                                setTable oldTable
--                                                return $ Match name ps' rhs' maybeBinds -- TODO:bindsも同様にスコープを作る。
--   infer ms@(InfixMatch _ _ _ _ _) = error "[Match - infer@infer.hs] The alpha-inferExp function is not defined for a given expression."

-- instance Typable (Rhs) where
--   infer (UnGuardedRhs e) = do e' <- infer e
--                                  return $ UnGuardedRhs e'
--   infer rhs@(GuardedRhss guardedRhs) = error "[Rhs - infer@infer.hs] The alpha-inferExp function is not defined for a given expression."

-- instance Typable (QOp) where
--   infer (QVarOp qName) = return $ QVarOp qName
--   infer (QConOp qName) = return $ QConOp qName