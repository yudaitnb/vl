module Syntax.Common.Literal (
  Literal(..),
) where

import Language.Haskell.Exts.Syntax
import Syntax.Common.Name

import Util

instance HasName (Literal l) where
  getName (Char _ _ str) = str
  getName (String _ _ str) = str
  getName (Int _ _ str) = str
  getName _ = error "getName function is not defined for a given expresion." 

instance PrettyAST l => PrettyAST (Literal l) where
  ppP (Char _ c s) = ppP "\'" <> ppP s <> ppP "\'"
  ppP (String _ s1 s2) = ppP "\"" <> ppP s2 <> ppP "\""
  ppP (Int _ i s) = ppP s
