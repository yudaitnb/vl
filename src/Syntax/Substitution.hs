module Syntax.Substitution  where

import Data.Map ( Map, empty, unionWith )

import Syntax.Type ( Type )

type SubstMap = Map String Type

emptySubst :: SubstMap
emptySubst = empty

comp :: SubstMap -> SubstMap -> SubstMap
comp = unionWith comp'
  where
    comp' :: Type -> Type -> Type
    comp' ty1 ty2 = ty1 -- [TODO] unifyを使って代入をマージする
