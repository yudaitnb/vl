module Syntax.Kind where

import Util

data Kind
  = TypeKind
  | LabelsKind
  deriving (Eq,Ord,Show)

-----------------------

instance PrettyAST Kind where
  ppE TypeKind = ppE "Type"
  ppE LabelsKind = ppE "Labels"
  ppP TypeKind = ppP "Type"
  ppP LabelsKind = ppP "Labels"