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

instance HasName (Name l) where
  getName (Ident _ str) = str
  getName (Symbol _ str) = str

instance HasName (QName l) where
  getName (Qual _ _ name) = getName name
  getName (UnQual _ name) = getName name
  getName _ = error "The getName function is not defined for a given expresion."

------------------------------------

instance Pretty l => Pretty (QName l) where
  pretty (Qual srcLocInfo moduleName name) =
        nest 2 $ pretty "(Qual" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty moduleName <> line
    <+> pretty name <> pretty ")"
  pretty (UnQual srcLocInfo name) =
        nest 2 $ pretty "(UnQual" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty name <> pretty ")"
  pretty _ = error "prety printer is not define a given QName."

instance Pretty l => Pretty (Name l) where
  pretty (Ident srcLocInfo string) =
        nest 2 $ pretty "(Ident" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty "\"" <> pretty string <> pretty "\"" <> pretty ")"
  pretty (Symbol srcLocInfo string) =
        nest 2 $ pretty "(Symbol" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty "\"" <> pretty string <> pretty "\"" <> pretty ")"

instance Pretty l => Pretty (ModuleName l) where
  pretty (ModuleName srcLocInfo str) =
        nest 2 $ pretty "(ModuleName" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty str <> pretty ")"

------------------------------------

instance PrettyAST l => PrettyAST (QName l) where
  ppE = pretty
  ppP (Qual _ moduleName name) = ppP moduleName <> pretty "." <> ppP name
  ppP (UnQual _ name) = ppP name
  ppP _ = error "showPrtty is not defined for an given expression."

instance PrettyAST l => PrettyAST (Name l) where
  ppE = pretty
  ppP (Ident _ str) = pretty str
  ppP (Symbol _ str) = pretty str

instance PrettyAST l => PrettyAST (ModuleName l) where
  ppE = pretty
  ppP (ModuleName _ str) = pretty str