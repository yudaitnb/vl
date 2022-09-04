{-# LANGUAGE DeriveFunctor #-}
module Syntax.STLC (
  Module(..), ModuleHead(..), ExportSpecList(..), ExportSpec(..),
  ImportDecl(..), ImportSpecList(..), ImportSpec(..), Namespace(..),
  Decl(..),
  Exp(..),
  Pat(..),
  Literal(..), Sign(..),
  S.Annotated(..),
  module Syntax.Name
) where

import qualified Language.Haskell.Exts.Syntax as S
import Language.Haskell.Exts.SrcLoc
import Data.List
import Data.Maybe
import Util

import Syntax.Name

-- | A complete STLC source module.
data Module l
    = Module l (Maybe (ModuleHead l)) [ImportDecl l] [Decl l]
    -- ^ an ordinary STLC module
  deriving (Eq,Ord,Show,Functor)

getDecls :: Module l -> [Decl l]
getDecls (Module l _ _ decls) = decls

-- | The head of a module, including the name and export specification.
data ModuleHead l = ModuleHead l (ModuleName l) (Maybe (ExportSpecList l))
  deriving (Eq,Ord,Show,Functor)

-- | An explicit export specification.
data ExportSpecList l
    = ExportSpecList l [ExportSpec l]
  deriving (Eq,Ord,Show,Functor)

-- | An item in a module's export specification.
data ExportSpec l
     = EVar l (QName l)                 -- ^ variable.
     | EAbs l (Namespace l) (QName l)   -- ^ @T@:
                                        --   a class or datatype exported abstractly,
                                        --   or a type synonym.
     | EModuleContents l (ModuleName l) -- ^ @module M@:
                                        --   re-export a module.
  deriving (Eq,Ord,Show,Functor)

-- | An import declaration.
data ImportDecl l = ImportDecl
    { importAnn :: l                   -- ^ annotation, used by parser for position of the @import@ keyword.
    , importModule :: ModuleName l     -- ^ name of the module imported.
    , importQualified :: Bool          -- ^ imported @qualified@?
    , importSrc :: Bool                -- ^ imported with @{-\# SOURCE \#-}@?
    , importSafe :: Bool               -- ^ Import @safe@?
    , importPkg :: Maybe String        -- ^ imported with explicit package name
    , importAs :: Maybe (ModuleName l) -- ^ optional alias name in an @as@ clause.
    , importSpecs :: Maybe (ImportSpecList l)
            -- ^ optional list of import specifications.
    }
  deriving (Eq,Ord,Show,Functor)

-- | An explicit import specification list.
data ImportSpecList l
    = ImportSpecList l Bool [ImportSpec l]
            -- ^ A list of import specifications.
            -- The 'Bool' is 'True' if the names are excluded
            -- by @hiding@.
  deriving (Eq,Ord,Show,Functor)

-- | An import specification, representing a single explicit item imported
--   (or hidden) from a module.
data ImportSpec l
     = IVar l (Name l)                  -- ^ variable
  deriving (Eq,Ord,Show,Functor)

-- | Namespaces for imports/exports.
data Namespace l = NoNamespace l | TypeNamespace l | PatternNamespace l
  deriving (Eq,Ord,Show,Functor)

-- | A top-level declaration.
data Decl l
     = PatBind      l (Pat l) (Exp l)
     -- ^ A pattern binding
  deriving (Eq,Ord,Show,Functor)

-- | A pattern, to be matched against a value.
data Pat l
    = PVar l (Name l)                       -- ^ variable
    | PLit l (Sign l) (Literal l)           -- ^ literal constant
    | PWildCard l                           -- ^ wildcard pattern: @_@
  deriving (Eq,Ord,Show,Functor)

-- | An indication whether a literal pattern has been negated or not.
data Sign l
    = Signless l
    | Negative l
  deriving (Eq,Ord,Show,Functor)

-- | STLC expressions.
data Exp l
    = Var l (QName l)                       -- ^ variable
    | Lit l (Literal l)                     -- ^ literal constant
    | App l (Exp l) (Exp l)                 -- ^ ordinary application
    | NegApp l (Exp l)                      -- ^ negation expression @-/exp/@ (unary minus)
    | Lambda l (Pat l) (Exp l)              -- ^ lambda expression
    | If l (Exp l) (Exp l) (Exp l)          -- ^ @if@ /exp/ @then@ /exp/ @else@ /exp/
    -- | Case l (Exp l) [Alt l]                -- ^ @case@ /exp/ @of@ /alts/
    -- | Paren l (Exp l)                       -- ^ parenthesised expression
    -- Versioned expressions
    -- LambdaCase
    -- | LCase l [Alt l]                       -- ^ @\case@ /alts/
  deriving (Eq,Ord,Show,Functor)

-- | /literal/
-- Values of this type hold the abstract value of the literal, along with the
-- precise string representation used.  For example, @10@, @0o12@ and @0xa@
-- have the same value representation, but each carry a different string representation.
data Literal l
    = Char       l Char     String     -- ^ character literal
    | String     l String   String     -- ^ string literal
    | Int        l Integer  String     -- ^ integer literal
  deriving (Eq,Ord,Show,Functor)

instance HasName (Pat l) where
  getName (PVar _ name) = getName name
  getName _             = "Patterns without PVar do not have a name field."

instance S.Annotated Module where
  ann (Module l _ _ _) = l
  amap f (Module l mmh iss dcls) = Module (f l) mmh iss dcls

instance S.Annotated ModuleHead where
  ann (ModuleHead l _ _)         = l
  amap f (ModuleHead l n mesl) = ModuleHead (f l) n mesl

instance S.Annotated ExportSpecList where
  ann (ExportSpecList l _)      = l
  amap f (ExportSpecList l ess) = ExportSpecList (f l) ess

instance S.Annotated ExportSpec where
  ann es = case es of
    EVar l _            -> l
    EAbs l _ _          -> l
    EModuleContents l _ -> l
  amap f es = case es of
    EVar l qn     -> EVar (f l) qn
    EAbs l n qn       -> EAbs (f l) n qn
    EModuleContents l mn    -> EModuleContents (f l) mn

instance S.Annotated ImportDecl where
  ann (ImportDecl l _ _ _ _ _ _ _) = l
  amap f (ImportDecl l mn qual src safe pkg mmn mis) = ImportDecl (f l) mn qual src safe pkg mmn mis

instance S.Annotated ImportSpecList where
    ann (ImportSpecList l _ _)      = l
    amap f (ImportSpecList l b iss) = ImportSpecList (f l) b iss

instance S.Annotated ImportSpec where
  ann is = case is of
    IVar l _         -> l
    -- IAbs l _ _       -> l
  amap f is = case is of
    IVar l n        -> IVar (f l) n
    -- IAbs l ns n     -> IAbs (f l) ns n

instance S.Annotated Namespace  where
  ann es = case es of
    NoNamespace l   -> l
    TypeNamespace l -> l
    PatternNamespace l -> l
  amap f es = case es of
    NoNamespace l   -> NoNamespace (f l)
    TypeNamespace l -> TypeNamespace (f l)
    PatternNamespace l -> PatternNamespace (f l)

instance S.Annotated Decl where
  ann (PatBind l _ _) = l
  amap f (PatBind l p exp) = PatBind (f l) p exp

instance S.Annotated Pat where
  ann (PVar l _) = l
  ann (PLit l _ _) = l
  ann (PWildCard l) = l
  amap f p1 = case p1 of
    PVar l n          -> PVar (f l) n
    PLit l sg lit     -> PLit (f l) sg lit
    PWildCard l       -> PWildCard (f l)

instance S.Annotated Sign where
  ann sg = case sg of
    Signless l -> l
    Negative l -> l
  amap = fmap

instance S.Annotated Literal where
  ann lit = case lit of
    Char    l _    _  -> l
    String  l _    _  -> l
    Int     l _    _  -> l
  amap = fmap

instance S.Annotated Exp where
  ann e = case e of
    Var l _                -> l
    Lit l _                -> l
    App l _ _              -> l
    NegApp l _             -> l
    Lambda l _ _           -> l
    If l _ _ _             -> l

  amap f e1 = case e1 of
    Var l qn        -> Var (f l) qn
    Lit l lit       -> Lit (f l) lit
    App l e1' e2    -> App (f l) e1' e2
    NegApp l e      -> NegApp (f l) e
    Lambda l ps e   -> Lambda (f l) ps e
    If l ec et ee   -> If (f l) ec et ee

class HasVar a where
  freeVars :: a -> [String]

instance HasVar (Exp l) where
  freeVars exp = 
    case exp of
      var@(Var l1 (UnQual l2 (Ident l3 str)))
                    -> [str]
      var@(Var _ _) -> []
      lit@(Lit _ _) -> []
      App _ e1 e2   -> freeVars e1 ++ freeVars e2
      NegApp _ e    -> freeVars e
      If _ e1 e2 e3 -> freeVars e1 ++ freeVars e2 ++ freeVars e3
      -- Lambda _ ps e -> freeVars e \\ boundVarsPats ps 
      _             -> error "The freeVars function is not defined for a given expression."

boundVarsPats :: [Pat l] -> [String]
boundVarsPats = foldl (\acc p -> boundVars p ++ acc) []

boundVars :: Pat l -> [String]
boundVars (PVar _ name) = [getName name]
boundVars (PLit {})  = [] 
boundVars (PWildCard _) = [] 

---------------------
--  Pretty printer --
---------------------

instance PrettyAST l => PrettyAST (Module l) where
  ppE (Module srcLocInfo moduleHead importDecl decl) =
        nest 2 $ parens $ ppE "Module" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE moduleHead <> line
    <+> pplist ppE importDecl <> line
    <+> pplist ppE decl
  ppP (Module srcLocInfo moduleHead importDecl decl) =
        ppP "module" <+> ppP moduleHead <+> ppP "where" <> line <> concatWith (surround line) (map ppP decl)
instance PrettyAST l => PrettyAST (ModuleHead l) where
  ppE (ModuleHead srcLocInfo moduleName exportSpecList) =
        nest 2 $ parens $ ppE "ModuleHead " <> line
    <+> ppE srcLocInfo <> line
    <+> ppE moduleName <> line
    <+> ppE exportSpecList
  ppP (ModuleHead srcLocInfo moduleName exportSpecList) = ppP moduleName
instance PrettyAST l => PrettyAST (ExportSpecList l) where
  ppE (ExportSpecList srcLocInfo exportSpec) =
        nest 2 $ parens $ ppE "ExportSpecList" <> line
    <+> ppE srcLocInfo <> line
    <+> pplist ppE exportSpec
  ppP (ExportSpecList srcLocInfo exportSpec) = list $ map ppP exportSpec
instance PrettyAST l => PrettyAST (ExportSpec l) where
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
  ppP (EVar srcLocInfo qName) = ppP qName
  ppP (EAbs srcLocInfo nameSpace qName) = ppP nameSpace <+> ppP qName
  ppP (EModuleContents srcLocInfo moduleName) = ppP moduleName
instance PrettyAST l => PrettyAST (ImportDecl l) where
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
  ppP (ImportDecl srcLocInfo importModule importQualified importSrc importSafe importPkg importAs importSpecs) =
        nest 2 $ parens $ ppP "ImportDecl" <> line
    <+> ppP srcLocInfo <> line
    <+> ppP importModule <> line
    <+> ppP importQualified <> line
    <+> ppP importSrc <> line
    <+> ppP importSafe <> line
    <+> ppP importPkg <> line
    <+> ppP importAs <> line
    <+> ppP importSpecs
instance PrettyAST l => PrettyAST (ImportSpecList l) where
  ppE (ImportSpecList srcLocInfo bool importSpecs) =
        nest 2 $ parens $ ppE "ImportSpecList" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE bool <> line
    <+> pplist ppE importSpecs
  ppP (ImportSpecList srcLocInfo bool importSpecs) =
        nest 2 $ parens $ ppP "ImportSpecList" <> line
    <+> ppP srcLocInfo <> line
    <+> ppP bool <> line
    <+> list (map ppP importSpecs)

instance PrettyAST l => PrettyAST (ImportSpec l) where
  ppE (IVar srcLocInfo name) =
        nest 2 $ parens $ ppE "IVar" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE name
  ppP (IVar srcLocInfo name) =
        nest 2 $ parens $ ppP "IVar" <> line
    <+> ppP srcLocInfo <> line
    <+> ppP name

instance PrettyAST l => PrettyAST (Namespace l)  where
  ppE (NoNamespace srcLocInfo) =
        parens $ ppE "NoNamespace" <> line
    <+> ppE srcLocInfo
  ppE (TypeNamespace srcLocInfo) =
        parens $ ppE "TypeNamespace" <> line
    <+> ppE srcLocInfo
  ppE (PatternNamespace srcLocInfo) =
        parens $ ppE "PatternNamespace" <> line
    <+> ppE srcLocInfo
  ppP (NoNamespace srcLocInfo) =
        parens $ ppP "NoNamespace" <> line
    <+> ppP srcLocInfo
  ppP (TypeNamespace srcLocInfo) =
        parens $ ppP "TypeNamespace" <> line
    <+> ppP srcLocInfo
  ppP (PatternNamespace srcLocInfo) =
        parens $ ppP "PatternNamespace" <> line
    <+> ppP srcLocInfo

instance PrettyAST l => PrettyAST (Decl l) where
  ppE (PatBind srcLocInfo pat rhd) =
        nest 2 $ parens $ ppE "PatBind" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE pat <> line
    <+> ppE rhd
  ppP (PatBind srcLocInfo pat rhd) = ppP pat <+> ppP "=" <+> ppP rhd <> line

instance PrettyAST l => PrettyAST (Exp l) where
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
  ppE (If srcLocInfo exp1 exp2 exp3) =
        nest 2 $ parens $ ppE "If" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE exp1 <> line
    <+> ppE exp2 <> line
    <+> ppE exp3
  ppE (Lambda srcLocInfo pat exp) =
        nest 2 $ parens $ ppE "Lambda" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE pat <> line
    <+> ppE exp
  ppP (Var _ qname) = ppP qname
  ppP (Lit _ literal) = ppP literal
  ppP (App _ e1 e2) = parens $ ppP e1 <+> ppP e2
  ppP (NegApp _ exp) = ppP "-" <> parens (ppP exp)
  ppP (If _ e1 e2 e3) = parens $ ppP "If" <+> ppP e1 <+> ppP "then"  <+> ppP e2 <+> ppP "else" <+> ppP e3
  ppP (Lambda _ p e) = parens $ backslash <> ppP p <> dot <> ppP e

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

instance PrettyAST l => PrettyAST (Pat l) where
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
  ppP (PVar _ name) = ppP name
  ppP (PLit _ sign literal) = ppP sign <> ppP literal
  ppP (PWildCard _) = ppP "_"

instance PrettyAST l => PrettyAST (Sign l) where
  ppE (Signless srcLocInfo) =
        parens $ ppE "Signless" <> line
    <+> ppE srcLocInfo
  ppE (Negative srcLocInfo) =
        parens $ ppE "Negative" <> line
    <+> ppE srcLocInfo
  ppP (Signless _) = ppP ""
  ppP (Negative _) = ppP "-"