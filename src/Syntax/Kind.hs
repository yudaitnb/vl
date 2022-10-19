module Syntax.Kind where

import Util

data Kind
  = TypeKind
  | LabelsKind
  | ConstraintKind
  deriving (Eq,Ord,Show)

-----------------------

instance PrettyAST Kind where
  ppE TypeKind = ppE "Type"
  ppE LabelsKind = ppE "Labels"
  ppE ConstraintKind = ppE "Constraint"
  ppP TypeKind = ppP "Type"
  ppP LabelsKind = ppP "Labels"
  ppP ConstraintKind = ppP "Constraint"