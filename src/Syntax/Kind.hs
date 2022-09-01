module Syntax.Kind where

import Util

data Kind
  = TypeKind
  | LabelsKind
  deriving (Eq,Ord,Show)

-----------------------

instance Pretty Kind where
  pretty TypeKind = pretty "Type"
  pretty LabelsKind = pretty "Labels"

-----------------------

instance PrettyAST Kind where
  ppE = pretty
  ppP = pretty