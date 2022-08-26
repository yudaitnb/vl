{-# LANGUAGE TypeFamilies #-}
module Desugar where

import qualified Syntax.STLC as STLC
import qualified Syntax.Absyn as Absyn

desugarAST :: Absyn.Module l -> STLC.Module l
desugarAST (Absyn.Module l mh _ imp decls) = 
  let
    mh' = fmap desugar mh
    imp' = fmap desugar imp
    decls' = map desugar $ Absyn.decomposeDecl decls
  in
    STLC.Module l mh' imp' decls'
desugarAST _ = error ""

class Desugaring ast where
  type Desugared ast
  desugar :: ast -> Desugared ast

instance Desugaring (Absyn.Module l) where
  type Desugared (Absyn.Module l) = (STLC.Module l)
  desugar (Absyn.Module l moduleHead _ importDecl decl) = STLC.Module l (fmap desugar moduleHead) (fmap desugar importDecl) (fmap desugar decl)
  desugar _ = error "The desugaring translation is not defined for a given expression."

instance Desugaring (Absyn.ModuleHead l) where
  type Desugared (Absyn.ModuleHead l) = (STLC.ModuleHead l)
  desugar (Absyn.ModuleHead l moduleName _ maybeExportSpecList) = STLC.ModuleHead l (desugar moduleName) (fmap desugar maybeExportSpecList)

instance Desugaring (Absyn.ExportSpecList l) where
  type Desugared (Absyn.ExportSpecList l) = (STLC.ExportSpecList l)
  desugar (Absyn.ExportSpecList l exportSpec) = STLC.ExportSpecList l (fmap desugar exportSpec)

instance Desugaring (Absyn.ExportSpec l) where
  type Desugared (Absyn.ExportSpec l) = (STLC.ExportSpec l)
  desugar (Absyn.EVar l qName) = STLC.EVar l (desugar qName)
  desugar (Absyn.EAbs l nameSpace qName) = STLC.EAbs l (desugar nameSpace) (desugar qName)
  desugar (Absyn.EModuleContents l moduleName) = STLC.EModuleContents l (desugar moduleName)
  desugar _ = error "[ExportSpec@Desugared.hs] The desugaring translation is not defined for a given expression."

instance Desugaring (Absyn.ImportDecl l) where
  type Desugared (Absyn.ImportDecl l) = (STLC.ImportDecl l)
  desugar (Absyn.ImportDecl importAnn importModule importQualified importSrc importSafe importPkg importAs importSpecs)
    = STLC.ImportDecl importAnn (desugar importModule) importQualified importSrc importSafe importPkg (fmap desugar importAs) (fmap desugar importSpecs)

instance Desugaring (Absyn.ImportSpecList l) where
  type Desugared (Absyn.ImportSpecList l) = (STLC.ImportSpecList l)
  desugar (Absyn.ImportSpecList l bool importSpec) = STLC.ImportSpecList l bool (fmap desugar importSpec)

instance Desugaring (Absyn.ImportSpec l) where
  type Desugared (Absyn.ImportSpec l) = (STLC.ImportSpec l)
  desugar (Absyn.IVar l name) = STLC.IVar l (desugar name)
  desugar _ = error "[ImportSpec@Desugared.hs] The desugaring translation is not defined for a given expression."

instance Desugaring (Absyn.Namespace l) where
  type Desugared (Absyn.Namespace l) = (STLC.Namespace l)
  desugar (Absyn.NoNamespace l) = STLC.NoNamespace l
  desugar (Absyn.TypeNamespace l) = STLC.TypeNamespace l
  desugar (Absyn.PatternNamespace l) = STLC.PatternNamespace l

instance Desugaring (Absyn.ModuleName l) where
  type Desugared (Absyn.ModuleName l) = (STLC.ModuleName l)
  desugar (Absyn.ModuleName l string) = STLC.ModuleName l string

instance Desugaring (Absyn.Name l) where
  type Desugared (Absyn.Name l) = (STLC.Name l)
  desugar (Absyn.Symbol l string) = STLC.Symbol l string
  desugar (Absyn.Ident l string) = STLC.Ident l string

instance Desugaring (Absyn.QName l) where
  type Desugared (Absyn.QName l) = (STLC.QName l)
  desugar (Absyn.Qual l moduleName name) = STLC.Qual l (desugar moduleName) (desugar name)
  desugar (Absyn.UnQual l name) = STLC.UnQual l (desugar name)
  desugar _ = error "[QName@Desugar.hs] The desugaring translation is not defined for a given expression."

instance Desugaring (Absyn.Literal l) where
  type Desugared (Absyn.Literal l) = (STLC.Literal l)
  desugar (Absyn.Char l char string) = STLC.Char l char string
  desugar (Absyn.String l string1 string2) = STLC.String l string1 string2
  desugar (Absyn.Int l integer string) = STLC.Int l integer string
  desugar _ = error "[Literal@Desugar.hs] The desugaring translation is not defined for a given expression."

instance Desugaring (Absyn.Pat l) where
  type Desugared (Absyn.Pat l) = (STLC.Pat l)
  desugar (Absyn.PVar l name) =  STLC.PVar l (desugar name)
  desugar (Absyn.PLit l sign literal) = STLC.PLit l (desugar sign) (desugar literal)
  -- desugar (Absyn.PWildCard l) = PWildCard l
  desugar _ = error "[Pat@Desugar.hs] The desugaring translation is not defined for a given expression."

instance Desugaring (Absyn.Sign l) where
  type Desugared (Absyn.Sign l) = (STLC.Sign l)
  desugar (Absyn.Signless l) = STLC.Signless l
  desugar (Absyn.Negative l) = STLC.Negative l


-- Multiple top-level functions can be thought of as one big recursive let binding:
-- | f0 x0 = e0
-- | f1 x1 = e1
-- | main = e2
-- ... is equivalent to:
-- | main = let f0 x0 = e0
-- |            f1 x1 = e1
-- |        in  e2

-- Functions are equivalent to lambdas:
-- | f x y z = e
-- ... is equivalent to:
-- | f = \x y z -> e
-- ... which in turn desugars to:
-- | f = \x -> \y -> \z -> e
instance Desugaring (Absyn.Decl l) where
  type Desugared (Absyn.Decl l) = STLC.Decl l
  desugar fb@(Absyn.FunBind l match) =
    case match of
      []  -> error ""
      [m] ->
        --   desugar (name ps = rhs)
        -- = name = desugar (\ps -> rhs)
        let Absyn.Match l1 name ps (Absyn.UnGuardedRhs l2 exp) _ = Absyn.rmWhere m
            patName = STLC.PVar (Absyn.ann name) (desugar name)
            lam' = desugar $ Absyn.Lambda l1 ps exp
        in STLC.PatBind l1 patName lam'
      m:ms -> error "Before desugaring FunDecl, decompose it."
  desugar pb@(Absyn.PatBind {}) =
    let
      Absyn.PatBind l1 pat (Absyn.UnGuardedRhs l2 exp) Nothing  = Absyn.rmWhere pb
      pat' = desugar pat
      exp' = desugar exp
    in STLC.PatBind l1 pat' exp'
  desugar _ = error "The desugaring translation is not defined for a given expression."

instance Desugaring (Absyn.Rhs l) where
  type Desugared (Absyn.Rhs l) = STLC.Exp l
  desugar (Absyn.UnGuardedRhs l1 exp) = desugar exp
  desugar _ = error ""

instance Desugaring (Absyn.Exp l) where
  type Desugared (Absyn.Exp l) = STLC.Exp l
  desugar (Absyn.Var l qName) = STLC.Var l (desugar qName)
  desugar (Absyn.Lit l literal) = STLC.Lit l (desugar literal)
  desugar (Absyn.App l exp1 exp2) = STLC.App l (desugar exp1) (desugar exp2)
  desugar (Absyn.NegApp l exp) = STLC.NegApp l (desugar exp)
  desugar (Absyn.If l exp1 exp2 exp3) = STLC.If l (desugar exp1) (desugar exp2) (desugar exp3)
  -- desugar (e1 + e2) = ((+) (desugar e1)) (desugar e2)
  desugar (Absyn.InfixApp l1 e1 (Absyn.QVarOp l2 qName) e2) =
    let varOp = STLC.Var l2 (desugar qName)
        e1' = desugar e1
        e2' = desugar e2
    in STLC.App l1 (STLC.App l2 varOp e1') e2'
  -- desugar (\_ -> e)      = error
  -- desugar (\[p] -> e)    = \p -> e
  -- desugar (\(p:ps) -> e) = \p -> desugar (\ps -> e)
  desugar (Absyn.Lambda l pat e) =
    case pat of
      []   -> error ""
      [p]  ->
        let e' = desugar e
            p' = desugar p
        in STLC.Lambda l p' e'
      p:ps ->
        let lam' = desugar (Absyn.Lambda l ps e)
            pat' = desugar p
        in STLC.Lambda l pat' lam'
  -- desugar (let [] in exp)          = desugar exp
  -- desugar $ let (x = y):binds in z = (\x -> desugar (let binds in z)) y
  desugar (Absyn.Let l1 binds exp) =
    case binds of
      Absyn.BDecls l2     [] -> desugar exp
      Absyn.BDecls l2 (b:bs) ->
        let zexp' = desugar $ Absyn.Let l1 (Absyn.BDecls l2 bs) exp
            STLC.PatBind l1 xpat yexp = desugar b
            lam  = STLC.Lambda l1 xpat zexp'
        in STLC.App l1 lam yexp
      _                      -> error ""
  desugar _ = error ""
