{-# LANGUAGE FlexibleInstances #-}
module Syntax.Absyn (
  module Language.Haskell.Exts.Syntax,
  HasName(..),
  HasWhere(..),
  decomposeDecl
) where

import Language.Haskell.Exts.Syntax hiding (Name, QName, ModuleName)

import Syntax.Name
import Syntax.SrcLoc

import Util
import Language.Haskell.Exts (exactPrint)

instance HasName (Exp l) where
  getName (Var _ qName) = getName qName
  getName (Lit _ literal) = getName literal
  getName _ = error "Expressions without Var/Literal do not have a name field." 

instance HasName (Literal l) where
  getName (Char _ _ str) = str
  getName (String _ _ str) = str
  getName (Int _ _ str) = str
  getName _ = error "The getName function is not defined for a given expresion." 

instance HasName (Pat l) where
  getName (PVar _ name) = getName name
  getName _             = error "Patterns without PVar do not have a name field."

class HasWhere a where
  rmWhere :: a -> a

instance HasWhere (Match l) where
  rmWhere match@(Match l1 name pats (UnGuardedRhs l2 exp) maybeBinds) =
    let patName = PVar (ann name) name in
    case maybeBinds of
      Nothing -> match
      Just bs ->
        let exp' = Let l1 bs exp
            rhs' = UnGuardedRhs l2 exp'
        in Match l1 name pats rhs' Nothing
  rmWhere _ = error ""

instance HasWhere (Decl l) where
  rmWhere pb@(PatBind l1 pat (UnGuardedRhs l2 exp) maybeBinds) =
    case maybeBinds of
      Nothing -> pb
      Just bs ->
        let exp' = Let l1 bs exp
            rhs' = UnGuardedRhs l2 exp'
        in PatBind l1 pat rhs' maybeBinds
  rmWhere (FunBind l match) = FunBind l (fmap rmWhere match)
  rmWhere _ = error "The rmWhere function is not defined for a given expression."

decomposeDecl :: [Decl l] -> [Decl l]
decomposeDecl = concatMap decompose
  where
    decompose :: Decl l -> [Decl l]
    decompose (FunBind l [])     = []
    decompose (FunBind l (m:ms)) = FunBind l [m] : decompose (FunBind l ms)
    decompose pb@(PatBind {})    = [pb]
    decompose _ = error "Declarations other than FunBind/PatBind exist."

---------------------
--  Pretty printer --
---------------------

instance PrettyAST (Module SrcSpanInfo) where
  ppE (Module srcLocInfo moduleHead _ importDecl decl) =
        nest 2 $ parens $ ppE "Module" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE moduleHead <> line
    <+> pplist ppE importDecl <> line
    <+> pplist ppE decl
  ppE _ = ppE "(Unknown - Module)"
  ppP m = ppP $ exactPrint m []

instance PrettyAST (ModuleHead SrcSpanInfo) where
  ppE (ModuleHead srcLocInfo moduleName _ exportSpecList) =
        nest 2 $ parens $ ppE "ModuleHead " <> line
    <+> ppE srcLocInfo <> line
    <+> ppE moduleName <> line
    <+> ppE exportSpecList
  ppP m = ppP $ exactPrint m []

instance PrettyAST (ExportSpecList SrcSpanInfo) where
  ppE (ExportSpecList srcLocInfo exportSpec) =
        nest 2 $ parens $ ppE "ExportSpecList" <> line
    <+> ppE srcLocInfo <> line
    <+> pplist ppE exportSpec
  ppP m = ppP $ exactPrint m []

instance PrettyAST (ExportSpec SrcSpanInfo) where
  ppE (EVar srcLocInfo qName) =
        nest 2 $ parens $ ppE "EVar" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE qName
  ppE (EAbs srcLocInfo nameSpace qName) =
        nest 2 $ parens $ ppE "EAbs" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE nameSpace <> line
    <+> ppE qName
  ppE (EModuleContents srcLocInfo moduleName) =
        nest 2 $ parens $ ppE "EModuleContents" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE moduleName
  ppE _ = ppE "(Unknown - ExportSpec)"
  ppP m = ppP $ exactPrint m []

instance PrettyAST (ImportDecl SrcSpanInfo) where
  ppE (ImportDecl srcLocInfo importModule importQualified importSrc importSafe importPkg importAs importSpecs) =
        nest 2 $ parens $ ppE "ImportDecl" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE importModule <> line
    <+> ppE importQualified <> line
    <+> ppE importSrc <> line
    <+> ppE importSafe <> line
    <+> ppE importPkg <> line
    <+> ppE importAs <> line
    <+> ppE importSpecs
  ppP m = ppP $ exactPrint m []

instance PrettyAST (ImportSpecList SrcSpanInfo) where
  ppE (ImportSpecList srcLocInfo bool importSpecs) =
        nest 2 $ parens $ ppE "ImportSpecList" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE bool <> line
    <+> pplist ppE importSpecs
  ppP m = ppP $ exactPrint m []

instance PrettyAST (ImportSpec SrcSpanInfo) where
  ppE (IVar srcLocInfo name) =
        nest 2 $ parens $ ppE "IVar" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE name
  ppE (IAbs srcLocInfo namespace name) =
        nest 2 $ parens $ ppE "IAbs" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE namespace <> line
    <+> ppE name
  ppE _ = ppE "(Unknown- ImportSpec)"
  ppP m = ppP $ exactPrint m []

instance PrettyAST (Namespace SrcSpanInfo)  where
  ppE (NoNamespace srcLocInfo) =
        parens $ ppE "NoNamespace" <> line
    <+> ppE srcLocInfo
  ppE (TypeNamespace srcLocInfo) =
        parens $ ppE "TypeNamespace" <> line
    <+> ppE srcLocInfo
  ppE (PatternNamespace srcLocInfo) =
        parens $ ppE "PatternNamespace" <> line
    <+> ppE srcLocInfo

instance PrettyAST (Decl SrcSpanInfo) where
  ppE (FunBind srcLocInfo match) =
        nest 2 $ parens $ ppE "FunBind" <> line
    <+> ppE srcLocInfo <> line
    <+> pplist ppE match
  ppE (PatBind srcLocInfo pat rhd binds) =
        nest 2 $ parens $ ppE "PatBind" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE pat <> line
    <+> ppE rhd <> line
    <+> ppE binds
  ppE _ = ppE "(Unknown - Decl)"
  ppP m = ppP $ exactPrint m []

instance PrettyAST (Pat SrcSpanInfo) where
  ppE (PVar srcLocInfo name) =
        nest 2 $ parens $ ppE "PVar" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE name
  ppE (PLit srcLocInfo sign literal) =
        nest 2 $ parens $ ppE "PLit" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE sign <> line
    <+> ppE literal
  ppE (PWildCard srcLocInfo) =
        nest 2 $ parens $ ppE "PWildCard" <> line
    <+> ppE srcLocInfo
  ppE _ = ppE "(Unknown - Pat)"
  ppP m = ppP $ exactPrint m []

instance PrettyAST (Match SrcSpanInfo) where
  ppE (Match srcLocInfo name pat rhs binds) =
        nest 2 $ parens $ ppE "Match" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE name <> line
    <+> pplist ppE pat <> line
    <+> ppE rhs <> line
    <+> ppE binds
  ppE _ = ppE "(Unknown - Match)"
  ppP m = ppP $ exactPrint m []

instance PrettyAST (Rhs SrcSpanInfo) where
  ppE (UnGuardedRhs srcLocInfo exp) =
        nest 2 $ parens $ ppE "UnGuardedRhs" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE exp
  ppE _ = ppE "(Unknown - Rhs)"
  ppP m = ppP $ exactPrint m []

instance PrettyAST (Binds SrcSpanInfo) where
  ppE (BDecls srcLocInfo decl) =
        nest 2 $ parens $ ppE "BDecls" <> line
    <+> ppE srcLocInfo <> line
    <+> pplist ppE decl
  ppE _ = ppE "(Unknown - Binds)"
  ppP m = ppP $ exactPrint m []

instance PrettyAST (Sign SrcSpanInfo) where
  ppE (Signless srcLocInfo) =
        parens $ ppE "Signless" <> line
    <+> ppE srcLocInfo
  ppE (Negative srcLocInfo) =
        parens $ ppE "Negative" <> line
    <+> ppE srcLocInfo
  ppP m = ppP $ exactPrint m []

instance PrettyAST (Literal SrcSpanInfo) where
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
  ppE _ = ppE "(Unknown - Literal)"
  ppP m = ppP $ exactPrint m []

instance PrettyAST (Exp SrcSpanInfo) where
  ppE (Var srcLocInfo qname) =
        nest 2 $ parens $ ppE "Var" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE qname
  ppE (Lit srcLocInfo literal) =
        nest 2 $ parens $ ppE "Lit" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE literal
  ppE (App srcLocInfo exp1 exp2) =
        nest 2 $ parens $ ppE "App" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE exp1 <> line
    <+> ppE exp2
  ppE (NegApp srcLocInfo exp) =
        nest 2 $ parens $ ppE "NegApp" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE exp
  ppE (Let srcLocInfo binds exp) =
        nest 2 $ parens $ ppE "Let" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE binds <> line
    <+> ppE exp
  ppE (If srcLocInfo exp1 exp2 exp3) =
        nest 2 $ parens $ ppE "If" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE exp1 <> line
    <+> ppE exp2 <> line
    <+> ppE exp3 
  ppE (Lambda srcLocInfo pats exp) =
        nest 2 $ parens $ ppE "Lambda" <> line
    <+> ppE srcLocInfo <> line
    <+> pplist ppE pats <> line
    <+> ppE exp
  ppE (InfixApp srcLocInfo exp1 qOp exp2) =
        nest 2 $ parens $ ppE "InfixApp" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE exp1 <> line
    <+> ppE qOp <> line
    <+> ppE exp2
  ppE _ = ppE "(Unknown - Exp)"
  ppP m = ppP $ exactPrint m []

instance PrettyAST (QOp SrcSpanInfo) where
  ppE (QVarOp srcLocInfo qName) =
        nest 2 $ parens $ ppE "QVarOp" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE qName
  ppE (QConOp srcLocInfo qName) =
        nest 2 $ parens $ ppE "QConOp" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE qName
  ppP m = ppP $ exactPrint m []