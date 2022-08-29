module Syntax.Absyn (
  module Language.Haskell.Exts.Syntax,
  HasName(..),
  HasWhere(..),
  decomposeDecl
) where

import Language.Haskell.Exts.Syntax hiding (Name, QName, ModuleName)

import Syntax.Name

import Prettyprinter

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

instance Pretty l => Pretty (Module l) where
  pretty (Module srcLocInfo moduleHead _ importDecl decl) =
        nest 2 $ pretty "(Module" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty moduleHead <> line
    <+> pretty importDecl <> line
    <+> pretty decl <> pretty ")"
  pretty _ = pretty "(Unknown - Module)"

instance Pretty l => Pretty (ModuleHead l) where
  pretty (ModuleHead srcLocInfo moduleName _ exportSpecList) =
        nest 2 $ pretty "(ModuleHead " <> line
    <+> pretty srcLocInfo <> line
    <+> pretty moduleName <> line
    <+> pretty exportSpecList <> pretty ")"

instance Pretty l => Pretty (ExportSpecList l) where
  pretty (ExportSpecList srcLocInfo exportSpec) =
        nest 2 $ pretty "(ExportSpecList" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty exportSpec <> pretty ")"

instance Pretty l => Pretty (ExportSpec l) where
  pretty (EVar srcLocInfo qName) =
        nest 2 $ pretty "(EVar" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty qName <> pretty ")"
  pretty (EAbs srcLocInfo nameSpace qName) =
        nest 2 $ pretty "(EAbs" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty nameSpace <> line
    <+> pretty qName <> pretty ")"
  pretty (EModuleContents srcLocInfo moduleName) =
        nest 2 $ pretty "(EModuleContents" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty moduleName <> pretty ")"
  pretty _ = pretty "(Unknown - ExportSpec)"

instance Pretty l => Pretty (ImportDecl l) where
  pretty (ImportDecl srcLocInfo importModule importQualified importSrc importSafe importPkg importAs importSpecs) =
        nest 2 $ pretty "(ImportDecl" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty importModule <> line
    <+> pretty importQualified <> line
    <+> pretty importSrc <> line
    <+> pretty importSafe <> line
    <+> pretty importPkg <> line
    <+> pretty importAs <> line
    <+> pretty importSpecs <> pretty ")"

instance Pretty l => Pretty (ImportSpecList l) where
  pretty (ImportSpecList srcLocInfo bool importSpecs) =
        nest 2 $ pretty "(ImportSpecList" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty bool <> line
    <+> pretty importSpecs <> pretty ")"

instance Pretty l => Pretty (ImportSpec l) where
  pretty (IVar srcLocInfo name) =
        nest 2 $ pretty "(IVar" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty name <> pretty ")"
  pretty (IAbs srcLocInfo namespace name) =
        nest 2 $ pretty "(IAbs" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty namespace <> line
    <+> pretty name <> pretty ")"
  pretty _ = pretty "(Unknown- ImportSpec)"

instance Pretty l => Pretty (Namespace l)  where
  pretty (NoNamespace srcLocInfo) =
        pretty "(NoNamespace" <> line
    <+> pretty srcLocInfo
    <> pretty ")"
  pretty (TypeNamespace srcLocInfo) =
        pretty "(TypeNamespace" <> line
    <+> pretty srcLocInfo
    <> pretty ")"
  pretty (PatternNamespace srcLocInfo) =
        pretty "(PatternNamespace" <> line
    <+> pretty srcLocInfo
    <> pretty ")"

instance Pretty l => Pretty (Decl l) where
  pretty (FunBind srcLocInfo match) =
        nest 2 $ pretty "(FunBind" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty match <> pretty ")"
  pretty (PatBind srcLocInfo pat rhd binds) =
        nest 2 $ pretty "(PatBind" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty pat <> line
    <+> pretty rhd <> line
    <+> pretty binds <> pretty ")"
  pretty _ = pretty "(Unknown - Decl)"

instance Pretty l => Pretty (Pat l) where
  pretty (PVar srcLocInfo name) =
        nest 2 $ pretty "(PVar" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty name <> pretty ")"
  pretty (PLit srcLocInfo sign literal) =
        nest 2 $ pretty "(PLit" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty sign <> line
    <+> pretty literal <> pretty ")"
  pretty (PWildCard srcLocInfo) =
        nest 2 $ pretty "(PWildCard" <> line
    <+> pretty srcLocInfo
    <> pretty ")"
  pretty _ = pretty "(Unknown - Pat)"

instance Pretty l => Pretty (Match l) where
  pretty (Match srcLocInfo name pat rhs binds) =
        nest 2 $ pretty "(Match" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty name <> line
    <+> pretty pat <> line
    <+> pretty rhs <> line
    <+> pretty binds <> pretty ")"
  pretty _ = pretty "(Unknown - Match)"

instance Pretty l => Pretty (Rhs l) where
  pretty (UnGuardedRhs srcLocInfo exp) =
        nest 2 $ pretty "(UnGuardedRhs" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty exp <> pretty ")"
  pretty _ = pretty "(Unknown - Rhs)"

instance Pretty l => Pretty (Binds l) where
  pretty (BDecls srcLocInfo decl) =
        nest 2 $ pretty "(BDecls" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty decl <> pretty ")"
  pretty _ = pretty "(Unknown - Binds)"

instance Pretty l => Pretty (Sign l) where
  pretty (Signless srcLocInfo) =
        pretty "(Signless" <> line
    <+> pretty srcLocInfo <> pretty ")"
  pretty (Negative srcLocInfo) =
        pretty "(Negative" <> line
    <+> pretty srcLocInfo <> pretty ")"

instance Pretty l => Pretty (Literal l) where
  pretty (Char srcLocInfo char string) =
        nest 2 $ pretty "(Char" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty "\'" <> pretty char
    <+> pretty "\"" <> pretty string <> pretty "\"" <> pretty ")"
  pretty (String srcLocInfo string1 string2) =
        nest 2 $ pretty "(String" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty "\"" <> pretty string1 <> pretty "\""
    <+> pretty "\"" <> pretty string2 <> pretty "\"" <> pretty ")"
  pretty (Int srcLocInfo integer string) =
        nest 2 $ pretty "(Int" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty integer
    <+> pretty "\"" <> pretty string <> pretty "\"" <> pretty ")"
  pretty _ = pretty "(Unknown - Literal)"

instance Pretty l => Pretty (Exp l) where
  pretty (Var srcLocInfo qname) =
        nest 2 $ pretty "(Var" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty qname <> pretty ")"
  pretty (Lit srcLocInfo literal) =
        nest 2 $ pretty "(Lit" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty literal <> pretty ")"
  pretty (App srcLocInfo exp1 exp2) =
        nest 2 $ pretty "(App" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty exp1 <> line
    <+> pretty exp2 <> pretty ")"
  pretty (NegApp srcLocInfo exp) =
        nest 2 $ pretty "(NegApp" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty exp <> pretty ")"
  pretty (Let srcLocInfo binds exp) =
        nest 2 $ pretty "(Let" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty binds <> line
    <+> pretty exp <> pretty ")"
  pretty (If srcLocInfo exp1 exp2 exp3) =
        nest 2 $ pretty "(If" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty exp1 <> line
    <+> pretty exp2 <> line
    <+> pretty exp3  <> pretty ")"
  pretty (Lambda srcLocInfo pats exp) =
        nest 2 $ pretty "(Lambda" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty pats <> line
    <+> pretty exp <> pretty ")"
  pretty (InfixApp srcLocInfo exp1 qOp exp2) =
        nest 2 $ pretty "(InfixApp" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty exp1 <> line
    <+> pretty qOp <> line
    <+> pretty exp2 <> pretty ")"
  pretty _ = pretty "(Unknown - Exp)"

instance Pretty l => Pretty (QOp l) where
  pretty (QVarOp srcLocInfo qName) =
        nest 2 $ pretty "(QVarOp" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty qName <> pretty ")"
  pretty (QConOp srcLocInfo qName) =
        nest 2 $ pretty "(QConOp" <> line
    <+> pretty srcLocInfo <> line
    <+> pretty qName <> pretty ")"