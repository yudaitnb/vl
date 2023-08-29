module Syntax.Common.Name (
  Language.Haskell.Exts.Syntax.QName(..),
  Language.Haskell.Exts.Syntax.Name(..),
  Language.Haskell.Exts.Syntax.ModuleName(..),
  Language.Haskell.Exts.Syntax.SpecialCon(..),
  HasName(..),
  mkQual, mkUnQual,
) where

import Language.Haskell.Exts.Syntax
import Syntax.Common.SrcLoc
import Syntax.Common.Keys
import Util

mkQual :: String -> String -> QName SrcSpanInfo
mkQual mn n = Qual noSrcSpan (ModuleName noSrcSpan mn) (Ident noSrcSpan n)

mkUnQual :: String -> QName SrcSpanInfo
mkUnQual n = UnQual noSrcSpan (Ident noSrcSpan n)

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

instance HasName VarKey where
  getName qk = case qk of
    QVar mn vn -> vn
    UQVar vn  -> vn

------------------------------------

instance (Show l, PrettyAST l) => PrettyAST (QName l) where
  ppP (Qual _ mn name) = ppP mn <> ppP "." <> ppP name
  ppP (UnQual _ name) = ppP name
  ppP (Special _ spc) = ppP spc

instance (Show l, PrettyAST l) => PrettyAST (SpecialCon l) where
  ppP sc = case sc of
    UnitCon l -> ppP "()"
    ListCon l -> ppP "[]"
    FunCon l  -> ppP "->"
    TupleCon l _ _ -> ppP "(,)" 
    Cons l    -> ppP ":"
    _ -> error $ "\nppP is not defined for an given expression.\n" ++ show sc  

instance PrettyAST l => PrettyAST (Name l) where
  ppP (Ident _ str) = ppP str
  ppP (Symbol _ str) = ppP str

instance PrettyAST l => PrettyAST (ModuleName l) where
  ppP (ModuleName _ str) = ppP str