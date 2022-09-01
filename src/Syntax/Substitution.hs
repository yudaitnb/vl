{-# LANGUAGE FlexibleInstances #-}
module Syntax.Substitution (
  SubstMap(..),
  emptySubst, singleSubst,
  comp
) where

import Data.Map ( Map, empty, unionWith, singleton, foldlWithKey, toList )

import Syntax.Type ( Type )
import Util

type SubstMap = Map String Type

emptySubst :: SubstMap
emptySubst = empty

singleSubst :: String -> Type -> SubstMap
singleSubst = singleton

comp :: SubstMap -> SubstMap -> SubstMap
comp = unionWith comp'
  where
    comp' :: Type -> Type -> Type
    comp' ty1 ty2 = ty1 -- [TODO] unifyを使って代入をマージする

--------------------------

instance Pretty SubstMap where
  pretty = foldlWithKey
    (\acc k v -> acc <> pretty ";" <+> 
      pretty k <> pretty "->" <> pretty v)
    emptyDoc

--------------------------

instance PrettyAST SubstMap where
  ppE = pretty
  ppP m = parens $ concatWith (surround comma) $ map (\(k,v) -> pretty k <> pretty "->" <> ppP v) (toList m)