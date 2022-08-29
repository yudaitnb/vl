module TypeUnification where

import Control.Monad.State

import Syntax.Type
import Syntax.Kind
import Syntax.Env
import Syntax.Substitution
import TypeSubstitution

typeUnification :: Type -> Type -> Env SubstMap
typeUnification ty1 ty2 =
  case (ty1, ty2) of
    -- U_→
    (TyFun a b, TyFun a' b') -> do
      theta_1 <- typeUnification a' a
      theta_2 <- typeUnification (typeSubstitution theta_1 b) (typeSubstitution theta_1 b')
      return $ comp theta_1 theta_2
    -- U_box
    (TyBox r a, TyBox r' a') -> do
      theta_1 <- typeUnification a a'
      theta_2 <- typeUnification (typeSubstitution theta_1 r) (typeSubstitution theta_1 r') -- [TODO] typeSubstitution不要かも
      return $ comp theta_1 theta_2
    -- U_var=, U_var∃
    (TyVar alpha, ty2) ->
      if ty1 == ty2
        then return emptySubst -- [TODO] well-definedness
        else return $ singleSubst (getName alpha)  ty2 -- [TODO] well-definedness
    -- U_=
    (ty1, ty2) ->
      if ty1 == ty2
        then return emptySubst -- [TODO] well-definedness
        else error ""