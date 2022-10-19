module Syntax.Label where

import Data.Map
import Data.List
import Data.Set
import Data.Monoid

import Syntax.Version
import Util

type Labels = Set Label

newtype Label = Label (Map String Version)
  deriving (Eq,Ord,Show)

emptyLabels :: Labels
emptyLabels = Data.Set.empty

------------------------

instance PrettyAST Label where
  ppE (Label m) = nest 2 $ ppE "(Label" <+> pplist ppE (Data.Map.toList m) <> ppE ")"
  ppP (Label m) = 
    parens $ concatWith (surround comma) $
      Data.List.map
        (\(k,v) -> ppP k <> ppP ":" <> ppP v)
        (Data.Map.toList m)

instance PrettyAST Labels where
  ppE set = concatWith (surround $ comma <> space) $ Data.List.map ppE (Data.Set.toList set)
  ppP set = concatWith (surround $ comma <> space) $ Data.List.map ppP (Data.Set.toList set)