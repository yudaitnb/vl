module Syntax.Name (
  Language.Haskell.Exts.Syntax.QName(..),
  Language.Haskell.Exts.Syntax.Name(..),
  Language.Haskell.Exts.Syntax.ModuleName(..),
  HasName(..)
) where

import Language.Haskell.Exts.Syntax
import Util

class HasName a where
  getName :: a -> String

instance HasName (ModuleName l) where
  getName (ModuleName _ str) = str

instance HasName (Name l) where
  getName (Ident _ str) = str
  getName (Symbol _ str) = str

instance HasName (QName l) where
  getName (Qual _ _ name) = getName name
  getName (UnQual _ name) = getName name
  getName _ = error "The getName function is not defined for a given expresion."

------------------------------------

instance PrettyAST l => PrettyAST (QName l) where
  ppE (Qual srcLocInfo moduleName name) =
        nest 2 $ ppE "(Qual" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE moduleName <> line
    <+> ppE name <> ppE ")"
  ppE (UnQual srcLocInfo name) =
        nest 2 $ ppE "(UnQual" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE name <> ppE ")"
  ppE _ = error "prety printer is not define a given QName."
  ppP (Qual _ moduleName name) = ppP moduleName <> ppP "." <> ppP name
  ppP (UnQual _ name) = ppP name
  ppP _ = error "showPrtty is not defined for an given expression."

instance PrettyAST l => PrettyAST (Name l) where
  ppE (Ident srcLocInfo string) =
        nest 2 $ ppE "(Ident" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE "\"" <> ppE string <> ppE "\"" <> ppE ")"
  ppE (Symbol srcLocInfo string) =
        nest 2 $ ppE "(Symbol" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE "\"" <> ppE string <> ppE "\"" <> ppE ")"
  ppP (Ident _ str) = ppP str
  ppP (Symbol _ str) = ppP str

instance PrettyAST l => PrettyAST (ModuleName l) where
  ppE (ModuleName srcLocInfo str) =
        nest 2 $ ppE "(ModuleName" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE str <> ppE ")"
  ppP (ModuleName _ str) = ppP str