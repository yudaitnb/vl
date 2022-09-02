module Inference.TypeUnification where

import Control.Monad.State

import Syntax.Type
import Syntax.Kind
import Syntax.Env
import Syntax.Substitution

import Util

putUnifyLog :: UEnv -> Type -> Type -> Subst -> Env ()
putUnifyLog oldu ty1 ty2 subst = do
  let header = pretty "(TyUnify)"
      env = header <+> ppP oldu <> vdash
      res = ppP ty1 <+> pretty "~" <+> ppP ty2 <+> pretty "▷" <+> ppP subst
  putLog $ putDocString $ env <> res
  return ()

typeUnification :: Type -> Type -> Env Subst
typeUnification ty1 ty2 = do
  sigma <- getUEnv
  case (ty1, ty2) of
    -- U_→
    (TyFun a b, TyFun a' b') -> do
      theta_1 <- typeUnification a' a
      theta_2 <- typeUnification (typeSubstitution theta_1 b) (typeSubstitution theta_1 b')
      let result = comp theta_1 theta_2
      putUnifyLog sigma ty1 ty2 result
      return result
    -- U_box
    (TyBox r a, TyBox r' a') -> do
      theta_1 <- typeUnification a a'
      theta_2 <- typeUnification (typeSubstitution theta_1 r) (typeSubstitution theta_1 r') -- [TODO] typeSubstitution不要かも
      let result = comp theta_1 theta_2
      putUnifyLog sigma ty1 ty2 result
      return result
    -- U_var=, U_var∃
    (TyVar alpha, ty2) ->
      if ty1 == ty2
        then do
          let result = emptySubst-- [TODO] well-definedness
          putUnifyLog sigma ty1 ty2 result
          return result
        else do
          let result = singleSubst (getName alpha) ty2 -- [TODO] well-definedness
          putUnifyLog sigma ty1 ty2 result
          return result
    (ty1, TyVar alpha) ->
      if ty1 == ty2
        then do
          let result = emptySubst -- [TODO] well-definedness
          putUnifyLog sigma ty1 ty2 result
          return result
        else do
          let result = singleSubst (getName alpha) ty1 -- [TODO] well-definedness
          putUnifyLog sigma ty1 ty2 result
          return result
    -- U_=
    (ty1, ty2) ->
      if ty1 == ty2
        then do
          let result = emptySubst -- [TODO] well-definedness
          putUnifyLog sigma ty1 ty2 result
          return result
        else do
          error ""
