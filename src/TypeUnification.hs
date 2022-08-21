module TypeUnification (unify) where

import Data.Map
import Control.Monad.State

import Syntax.Type
import Syntax.Kind
import Syntax.Substitution
import TypeSubstitution

type UEnv = Map String Kind

unify :: UEnv -> Type -> Type -> SubstMap
unify uenv ty1 ty2 = evalState (typeUnification ty1 ty2) (UnifyEnv' uenv)

newtype UnifyEnv' = UnifyEnv' { uEnv :: UEnv }
type UnifyEnv a = State UnifyEnv' a -- 参照しないのでReaderモナドで十分かも

primUnifyEnv :: UnifyEnv'
primUnifyEnv = UnifyEnv' empty

getUEnv :: UnifyEnv UEnv
getUEnv = state $ \(UnifyEnv' s) -> (s, UnifyEnv' s)

setUEnv :: UEnv -> UnifyEnv ()
setUEnv uenv = state $ \(UnifyEnv' _) -> ((), UnifyEnv' uenv)

typeUnification :: Type -> Type -> UnifyEnv SubstMap
typeUnification ty1 ty2 = case (ty1, ty2) of
  -- U_→
  (TyFun a b, TyFun a' b') -> do
    theta_1 <- typeUnification a' a
    theta_2 <- typeUnification (typeSubstitution theta_1 b) (typeSubstitution theta_1 b')
    return $ union theta_1 theta_2
  -- U_box
  (TyBox r1 a, TyBox r2 a') -> do
    theta_1 <- typeUnification a a'
    theta_2 <- typeUnification r1 r2
    return $ union theta_1 theta_2
  -- U_var=, U_var∃
  (TyVar n1, ty2) ->
    if ty1 == ty2
      then return empty -- [TODO] well-definedness
      else return $ fromList [(getName n1, ty2)] -- [TODO] well-definedness
  -- U_=
  (ty1, ty2) ->
    if ty1 == ty2
      then return empty -- [TODO] well-definedness
      else error ""