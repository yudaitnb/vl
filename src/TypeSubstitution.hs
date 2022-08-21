module TypeSubstitution where

import Control.Monad.State
import Data.Map

import Syntax.Type ( HasName(getName), Type(..) )
import Syntax.Substitution

typeSubstitution :: SubstMap -> Type -> Type
typeSubstitution subst ty = evalState (tysubst ty) (SubstEnv' subst)

newtype SubstEnv' = SubstEnv' { substitution :: SubstMap }
type SubstEnv a = State SubstEnv' a

primSubstEnv :: SubstEnv'
primSubstEnv = SubstEnv' empty

getSubst :: SubstEnv SubstMap
getSubst = state $ \(SubstEnv' s) -> (s, SubstEnv' s)

setSubst :: SubstMap -> SubstEnv ()
setSubst subst = state $ \(SubstEnv' _) -> ((), SubstEnv' subst)

tysubst :: Type -> SubstEnv Type
tysubst ty = case ty of
  TyCon qName -> return ty
  TyBottom    -> return ty
  TyLabels _  -> return ty
  TyVar name     -> do
    s_table <- getSubst
    let ty' = findWithDefault ty (getName name) s_table
    return ty'
  TyFun ty1 ty2  -> do
    ty1' <- tysubst ty1
    ty2' <- tysubst ty2
    return $ TyFun ty1' ty2'
  TyBox coeff ty -> do
    coeff' <- tysubst coeff
    ty'    <- tysubst ty
    return $ TyBox coeff' ty'