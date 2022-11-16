module Syntax.Common.Name (
  Language.Haskell.Exts.Syntax.QName(..),
  Language.Haskell.Exts.Syntax.Name(..),
  Language.Haskell.Exts.Syntax.ModuleName(..),
  Language.Haskell.Exts.Syntax.SpecialCon(..),
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

instance (Show l, PrettyAST l) => PrettyAST (QName l) where
  ppE (Qual srcLocInfo moduleName name) =
        nest 2 $ parens $ ppE "Qual" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE moduleName <> line
    <+> ppE name
  ppE (UnQual srcLocInfo name) =
        nest 2 $ parens $ ppE "UnQual" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE name
  ppE qn = error $ "\nppE is not defined for an given expression.\n" ++ show qn
  ppP (Qual _ mn name) = ppP mn <> ppP "." <> ppP name
  ppP (UnQual _ name) = ppP name
  ppP (Special _ spc) = ppP spc

instance (Show l, PrettyAST l) => PrettyAST (SpecialCon l) where
  ppE sc = case sc of
    UnitCon l -> ppE "()"
    ListCon l -> ppE "[]"
    FunCon l  -> ppE "->"
    TupleCon l _ _ -> ppE "(,)" 
    Cons l    -> ppE ":"
    _ -> error $ "\nppE is not defined for an given expression.\n" ++ show sc
  ppP sc = case sc of
    UnitCon l -> ppP "()"
    ListCon l -> ppP "[]"
    FunCon l  -> ppP "->"
    TupleCon l _ _ -> ppP "(,)" 
    Cons l    -> ppP ":"
    _ -> error $ "\nppP is not defined for an given expression.\n" ++ show sc  

instance PrettyAST l => PrettyAST (Name l) where
  ppE (Ident srcLocInfo string) =
        nest 2 $ parens $ ppE "Ident" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE "\"" <> ppE string <> ppE "\""
  ppE (Symbol srcLocInfo string) =
        nest 2 $ parens $ ppE "Symbol" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE "\"" <> ppE string <> ppE "\""
  ppP (Ident _ str) = ppP str
  ppP (Symbol _ str) = ppP str

instance PrettyAST l => PrettyAST (ModuleName l) where
  ppE (ModuleName srcLocInfo str) =
        nest 2 $ parens $ ppE "ModuleName" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE str
  ppP (ModuleName _ str) = ppP str