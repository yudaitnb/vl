module Syntax.Kind where

import Util

data Kind
  = TypeKind
  | LabelsKind
  | ConstraintKind
  deriving (Eq,Ord,Show)

-----------------------

instance PrettyAST Kind where
  ppP TypeKind = ppP "Type"
  ppP LabelsKind = ppP "Label"
  ppP ConstraintKind = ppP "Constraint"