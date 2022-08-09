{-# LANGUAGE TypeFamilies #-}
module Girard (
  girardFwd
) where

import qualified Language.Haskell.Exts.Syntax as S
import Language.Haskell.Exts.SrcLoc ( SrcSpanInfo )

import VL
import Language.Haskell.Exts (Annotated)

class GirardFwd ast where
  type Girard ast -- type family
  girardFwd :: ast -> Girard ast

instance GirardFwd (S.Module l) where
  type Girard (S.Module l) = (Module l)
  girardFwd (S.Module l moduleHead _ importDecl decl) = Module l (fmap girardFwd moduleHead) (fmap girardFwd importDecl) (fmap girardFwd decl)
  girardFwd _ = error "The girard's (forward) translation is not defined for a given expression."

instance GirardFwd (S.ModuleHead l) where
  type Girard (S.ModuleHead l) = (ModuleHead l)
  girardFwd (S.ModuleHead l moduleName _ maybeExportSpecList) = ModuleHead l (girardFwd moduleName) (fmap girardFwd maybeExportSpecList)

instance GirardFwd (S.ExportSpecList l) where
  type Girard (S.ExportSpecList l) = (ExportSpecList l)
  girardFwd (S.ExportSpecList l exportSpec) = ExportSpecList l (fmap girardFwd exportSpec)

instance GirardFwd (S.ExportSpec l) where
  type Girard (S.ExportSpec l) = (ExportSpec l)
  girardFwd (S.EVar l qName) = EVar l (girardFwd qName)
  girardFwd (S.EAbs l nameSpace qName) = EAbs l (girardFwd nameSpace) (girardFwd qName)
  girardFwd (S.EModuleContents l moduleName) = EModuleContents l (girardFwd moduleName)
  girardFwd _ = error "The girard's (forward) translation is not defined for a given expression."

instance GirardFwd (S.ImportDecl l) where
  type Girard (S.ImportDecl l) = (ImportDecl l)
  girardFwd (S.ImportDecl importAnn importModule importQualified importSrc importSafe importPkg importAs importSpecs)
    = ImportDecl importAnn (girardFwd importModule) importQualified importSrc importSafe importPkg (fmap girardFwd importAs) (fmap girardFwd importSpecs)

instance GirardFwd (S.ImportSpecList l) where
  type Girard (S.ImportSpecList l) = (ImportSpecList l)
  girardFwd (S.ImportSpecList l bool importSpec) = ImportSpecList l bool (fmap girardFwd importSpec)

instance GirardFwd (S.ImportSpec l) where
  type Girard (S.ImportSpec l) = (ImportSpec l)
  girardFwd (S.IVar l name) = IVar l (girardFwd name)
  girardFwd _ = error "The girard's (forward) translation is not defined for a given expression."

instance GirardFwd (S.Exp l) where
  type Girard (S.Exp l) = (Exp l)
  girardFwd (S.Var l qName) = Var l (girardFwd qName)
  girardFwd (S.Lit l literal) = Lit l (girardFwd literal)
  girardFwd (S.App l exp1 exp2) = App l (girardFwd exp1) (Pr (S.ann exp2) (girardFwd exp2))
  girardFwd (S.NegApp l exp) = NegApp l (girardFwd exp)
  girardFwd (S.Lambda l pat exp) = Lambda l (PBox <$> S.ann <*> girardFwd <$> pat) (girardFwd exp) -- girard $ S.Lambda l [x,y] t = Lambda l [[x],[y]] (girard t)
  -- girardFwd (S.Let l binds exp) = Let l (girardFwd binds) (girardFwd exp)
  girardFwd (S.If l exp1 exp2 exp3) = If l (girardFwd exp1) (girardFwd exp2) (girardFwd exp3)
  girardFwd _ = error "The girard's (forward) translation is not defined for a given expression."

instance GirardFwd (S.Namespace l) where
  type Girard (S.Namespace l) = (Namespace l)
  girardFwd (S.NoNamespace l) = NoNamespace l
  girardFwd (S.TypeNamespace l) = TypeNamespace l
  girardFwd (S.PatternNamespace l) = PatternNamespace l

instance GirardFwd (S.ModuleName l) where
  type Girard (S.ModuleName l) = (ModuleName l)
  girardFwd (S.ModuleName l string) = ModuleName l string

instance GirardFwd (S.Name l) where
  type Girard (S.Name l) = (Name l)
  girardFwd (S.Symbol l string) = Symbol l string
  girardFwd (S.Ident l string) = Ident l string

instance GirardFwd (S.QName l) where
  type Girard (S.QName l) = (QName l)
  girardFwd (S.Qual l moduleName name) = Qual l (girardFwd moduleName) (girardFwd name)
  girardFwd (S.UnQual l name) = UnQual l (girardFwd name)
  girardFwd _ = error "The girard's (forward) translation is not defined for a given expression."

instance GirardFwd (S.Literal l) where
  type Girard (S.Literal l) = (Literal l)
  girardFwd (S.Char l char string) = Char l char string
  girardFwd (S.String l string1 string2) = String l string1 string2
  girardFwd (S.Int l integer string) = Int l integer string
  girardFwd _ = error "The girard's (forward) translation is not defined for a given expression."

instance GirardFwd (S.Pat l) where
  type Girard (S.Pat l) = (Pat l)
  girardFwd (S.PVar l name) =  PVar l (girardFwd name)
  girardFwd (S.PLit l sign literal) = PLit l (girardFwd sign) (girardFwd literal)
  -- girardFwd (S.PWildCard l) = PWildCard l
  girardFwd _ = error "The girard's (forward) translation is not defined for a given expression."

instance GirardFwd (S.Sign l) where
  type Girard (S.Sign l) = (Sign l)
  girardFwd (S.Signless l) = Signless l
  girardFwd (S.Negative l) = Negative l

instance GirardFwd (S.Decl l) where
  type Girard (S.Decl l) = (Decl l)
  girardFwd (S.FunBind l match) = FunBind l (fmap girardFwd match)
  girardFwd (S.PatBind l pat rhs maybeBind) = PatBind l (girardFwd pat) (girardFwd rhs) (fmap girardFwd maybeBind)
  girardFwd _ = error "The girard's (forward) translation is not defined for a given expression."

instance GirardFwd (S.Binds l) where
  type Girard (S.Binds l) = (Binds l)
  girardFwd (S.BDecls l decls) = BDecls l (fmap girardFwd decls)
  girardFwd _ = error "The girard's (forward) translation is not defined for a given expression."

instance GirardFwd (S.Match l) where
  type Girard (S.Match l) = (Match l)
  girardFwd (S.Match l name pat rhs maybeBind) = Match l (girardFwd name) (fmap girardFwd pat) (girardFwd rhs) (fmap girardFwd maybeBind)
  girardFwd _ = error "The girard's (forward) translation is not defined for a given expression."

instance GirardFwd (S.Rhs l) where
  type Girard (S.Rhs l) = (Rhs l)
  girardFwd (S.UnGuardedRhs l exp) = UnGuardedRhs l (girardFwd exp)
  girardFwd _ = error "The girard's (forward) translation is not defined for a given expression."