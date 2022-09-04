module Syntax.LambdaVL (
  Module(..), ModuleHead(..), ExportSpecList(..), ExportSpec(..),
  ImportDecl(..), ImportSpecList(..), ImportSpec(..), Namespace(..),
  Decl(..),
  Exp(..),
  Pat(..),
  Literal(..), Sign(..),
  HasVar(..)
) where

import qualified Language.Haskell.Exts.Syntax as S
import Language.Haskell.Exts.SrcLoc
import Data.List
import Data.Maybe

import Syntax.Name
import Util

-- | A complete Lambda VL source module.
data Module l
    = Module l (Maybe (ModuleHead l)) [ImportDecl l] [Decl l]
  deriving (Eq,Ord,Show)

getDecls :: Module l -> [Decl l]
getDecls (Module l _ _ decls) = decls

-- | The head of a module, including the name and export specification.
data ModuleHead l = ModuleHead l (ModuleName l) (Maybe (ExportSpecList l))
  deriving (Eq,Ord,Show)

-- | An explicit export specification.
data ExportSpecList l
    = ExportSpecList l [ExportSpec l]
  deriving (Eq,Ord,Show)

-- | An item in a module's export specification.
data ExportSpec l
     = EVar l (QName l)                 -- ^ variable.
     | EAbs l (Namespace l) (QName l)   -- ^ @T@:
                                        --   a class or datatype exported abstractly,
                                        --   or a type synonym.
     | EModuleContents l (ModuleName l) -- ^ @module M@:
                                        --   re-export a module.
  deriving (Eq,Ord,Show)

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
  deriving (Eq,Ord,Show)

-- | An explicit import specification list.
data ImportSpecList l
    = ImportSpecList l Bool [ImportSpec l]
            -- ^ A list of import specifications.
            -- The 'Bool' is 'True' if the names are excluded
            -- by @hiding@.
  deriving (Eq,Ord,Show)

-- | An import specification, representing a single explicit item imported
--   (or hidden) from a module.
data ImportSpec l
     = IVar l (Name l)                  -- ^ variable
  deriving (Eq,Ord,Show)

-- | Namespaces for imports/exports.
data Namespace l = NoNamespace l | TypeNamespace l | PatternNamespace l
  deriving (Eq,Ord,Show)

-- | A top-level declaration.
data Decl l
     = PatBind      l (Pat l) (Exp l)
     -- ^ A pattern binding
  deriving (Eq,Ord,Show)

-- | A pattern, to be matched against a value.
data Pat l
    = PVar l (Name l)                       -- ^ variable
    | PLit l (Sign l) (Literal l)           -- ^ literal constant
    | PWildCard l                           -- ^ wildcard pattern: @_@
    | PBox l (Pat l)                        -- ^ promoted pattern; @[@/x/@]@ -> ...
  deriving (Eq,Ord,Show)

-- | An indication whether a literal pattern has been negated or not.
data Sign l
    = Signless l
    | Negative l
  deriving (Eq,Ord,Show)

-- | Lambda VL expressions.
data Exp l
    = Var l (QName l)                       -- ^ variable
    | Lit l (Literal l)                     -- ^ literal constant
    | App l (Exp l) (Exp l)                 -- ^ ordinary application
    | NegApp l (Exp l)                      -- ^ negation expression @-/exp/@ (unary minus)
    | Lambda l (Pat l) (Exp l)              -- ^ lambda expression
    | If l (Exp l) (Exp l) (Exp l)          -- ^ @if@ /exp/ @then@ /exp/ @else@ /exp/
    -- Versioned expressions
    | Pr l (Exp l)                          -- ^ An expression promoted version resources
    -- | Case l (Exp l) [Alt l]                -- ^ @case@ /exp/ @of@ /alts/
    -- | Paren l (Exp l)                       -- ^ parenthesised expression
    -- | LCase l [Alt l]                       -- ^ @\case@ /alts/
  deriving (Eq,Ord,Show)

-- | /literal/
-- Values of this type hold the abstract value of the literal, along with the
-- precise string representation used.  For example, @10@, @0o12@ and @0xa@
-- have the same value representation, but each carry a different string representation.
data Literal l
    = Char       l Char     String     -- ^ character literal
    | String     l String   String     -- ^ string literal
    | Int        l Integer  String     -- ^ integer literal
  deriving (Eq,Ord,Show)


instance HasName (Exp l) where
  getName (Var _ qName) = getName qName
  getName (Lit _ literal) = getName literal
  getName _ = error "Expressions without Var/Literal do not have a name field."

instance HasName (Literal l) where
  getName (Char _ _ str) = str
  getName (String _ _ str) = str
  getName (Int _ _ str) = str

instance HasName (Pat l) where
  getName (PVar _ name) = getName name
  getName _             = error "Patterns without PVar do not have a name field."

--
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
boundVars (PBox _ p)    = boundVars p

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
  ppE (Pr srcLocInfo exp) =
        nest 2 $ parens $ ppE "Pr" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE exp
  ppP (Var _ qname) = ppP qname
  ppP (Lit _ literal) = ppP literal
  ppP (App _ e1 e2) = parens $ ppP e1 <+> ppP e2
  ppP (NegApp _ exp) = ppP "-" <> parens (ppP exp)
  ppP (If _ e1 e2 e3) = parens $ ppP "If" <+> ppP e1 <+> ppP "then"  <+> ppP e2 <+> ppP "else" <+> ppP e3
  ppP (Lambda _ p e) = parens $ backslash <> ppP p <> dot <> ppP e
  ppP (Pr _ e) = brackets $ ppP e

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
  ppE (PBox srcLocInfo pat) =
        nest 2 $ parens $ ppE "PBox" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE pat
  ppP (PVar _ name) = ppP name
  ppP (PLit _ sign literal) = ppP sign <> ppP literal
  ppP (PWildCard _) = ppP "_"
  ppP (PBox _ pat) = brackets $ ppP pat

instance PrettyAST l => PrettyAST (Sign l) where
  ppE (Signless srcLocInfo) =
        parens $ ppE "Signless" <> line
    <+> ppE srcLocInfo
  ppE (Negative srcLocInfo) =
        parens $ ppE "Negative" <> line
    <+> ppE srcLocInfo
  ppP (Signless _) = ppP ""
  ppP (Negative _) = ppP "-"