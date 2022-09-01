{-# LANGUAGE TypeFamilies #-}
module Girard (
  girardFwd
) where

import qualified Syntax.STLC as STLC
import qualified Syntax.LambdaVL as VL
-- import Syntax.Type

class GirardFwd ast where
  type Girard ast -- type family
  girardFwd :: ast -> Girard ast

instance GirardFwd (STLC.Module l) where
  type Girard (STLC.Module l) = (VL.Module l)
  girardFwd (STLC.Module l moduleHead importDecl decl) = VL.Module l (fmap girardFwd moduleHead) (fmap girardFwd importDecl) (fmap girardFwd decl)

instance GirardFwd (STLC.ModuleHead l) where
  type Girard (STLC.ModuleHead l) = (VL.ModuleHead l)
  girardFwd (STLC.ModuleHead l moduleName maybeExportSpecList) = VL.ModuleHead l moduleName (fmap girardFwd maybeExportSpecList)

instance GirardFwd (STLC.ExportSpecList l) where
  type Girard (STLC.ExportSpecList l) = (VL.ExportSpecList l)
  girardFwd (STLC.ExportSpecList l exportSpec) = VL.ExportSpecList l (fmap girardFwd exportSpec)

instance GirardFwd (STLC.ExportSpec l) where
  type Girard (STLC.ExportSpec l) = (VL.ExportSpec l)
  girardFwd (STLC.EVar l qName) = VL.EVar l qName
  girardFwd (STLC.EAbs l nameSpace qName) = VL.EAbs l (girardFwd nameSpace) qName
  girardFwd (STLC.EModuleContents l moduleName) = VL.EModuleContents l moduleName

instance GirardFwd (STLC.ImportDecl l) where
  type Girard (STLC.ImportDecl l) = (VL.ImportDecl l)
  girardFwd (STLC.ImportDecl importAnn importModule importQualified importSrc importSafe importPkg importAs importSpecs)
    = VL.ImportDecl importAnn importModule importQualified importSrc importSafe importPkg importAs (fmap girardFwd importSpecs)

instance GirardFwd (STLC.ImportSpecList l) where
  type Girard (STLC.ImportSpecList l) = (VL.ImportSpecList l)
  girardFwd (STLC.ImportSpecList l bool importSpec) = VL.ImportSpecList l bool (fmap girardFwd importSpec)

instance GirardFwd (STLC.ImportSpec l) where
  type Girard (STLC.ImportSpec l) = (VL.ImportSpec l)
  girardFwd (STLC.IVar l name) = VL.IVar l name

instance GirardFwd (STLC.Exp l) where
  type Girard (STLC.Exp l) = (VL.Exp l)
  girardFwd (STLC.Var l qName) = VL.Var l qName
  girardFwd (STLC.Lit l literal) = VL.Lit l (girardFwd literal)
  girardFwd (STLC.App l exp1 exp2) = VL.App l (girardFwd exp1) (VL.Pr (STLC.ann exp2) (girardFwd exp2))
  girardFwd (STLC.NegApp l exp) = VL.NegApp l (girardFwd exp)
  girardFwd (STLC.Lambda l pat exp) = 
    let src = STLC.ann pat
        pat' = girardFwd pat
        pbox = VL.PBox src pat'
    in VL.Lambda l pbox (girardFwd exp)
  -- girardFwd (STLC.Let l binds exp) = Let l (girardFwd binds) (girardFwd exp)
  girardFwd (STLC.If l exp1 exp2 exp3) = VL.If l (girardFwd exp1) (girardFwd exp2) (girardFwd exp3)

instance GirardFwd (STLC.Namespace l) where
  type Girard (STLC.Namespace l) = (VL.Namespace l)
  girardFwd (STLC.NoNamespace l) = VL.NoNamespace l
  girardFwd (STLC.TypeNamespace l) = VL.TypeNamespace l
  girardFwd (STLC.PatternNamespace l) = VL.PatternNamespace l

instance GirardFwd (STLC.Literal l) where
  type Girard (STLC.Literal l) = (VL.Literal l)
  girardFwd (STLC.Char l char string) = VL.Char l char string
  girardFwd (STLC.String l string1 string2) = VL.String l string1 string2
  girardFwd (STLC.Int l integer string) = VL.Int l integer string

instance GirardFwd (STLC.Pat l) where
  type Girard (STLC.Pat l) = (VL.Pat l)
  girardFwd (STLC.PVar l name) =  VL.PVar l name
  girardFwd (STLC.PLit l sign literal) = VL.PLit l (girardFwd sign) (girardFwd literal)
  -- girardFwd (STLC.PWildCard l) = PWildCard l
  girardFwd _ = error "[Pat@Girard.hs] The girard's (forward) translation is not defined for a given expression."

instance GirardFwd (STLC.Sign l) where
  type Girard (STLC.Sign l) = (VL.Sign l)
  girardFwd (STLC.Signless l) = VL.Signless l
  girardFwd (STLC.Negative l) = VL.Negative l

instance GirardFwd (STLC.Decl l) where
  type Girard (STLC.Decl l) = (VL.Decl l)
  -- girardFwd (STLC.FunBind l match) = FunBind l (fmap girardFwd match)
  girardFwd (STLC.PatBind l pat exp) = VL.PatBind l (girardFwd pat) (girardFwd exp)

  