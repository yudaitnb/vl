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
  ppE (Char srcLocInfo char string) =
        nest 2 $ parens $ ppE "Char" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE "\'" <> ppE char
    <+> ppE "\"" <> ppE string <> ppE "\""
  ppE (String srcLocInfo string1 string2) =
        nest 2 $ parens $ ppE "String" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE "\"" <> ppE string1 <> ppE "\""
    <+> ppE "\"" <> ppE string2 <> ppE "\""
  ppE (Int srcLocInfo integer string) =
        nest 2 $ parens $ ppE "Int" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE integer
    <+> ppE "\"" <> ppE string <> ppE "\""
  ppP (Char _ c s) = ppP s
  ppP (String _ s1 s2) = ppP s2
  ppP (Int _ i s) = ppP s
