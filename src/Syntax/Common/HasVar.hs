module Syntax.Common.HasVar where

import Data.List ( nub )
import Syntax.Common.Keys

class HasVar a where
  freeVars :: a -> [VarKey]
  vars :: a -> [VarKey]

instance HasVar a => HasVar [a] where
  freeVars  xs = nub $ concatMap freeVars xs
  vars xs = nub $ concatMap vars xs
