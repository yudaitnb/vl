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
import Prettyprinter

import Syntax.Name

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
boundVars (PLit _ _ _)  = [] 
boundVars (PWildCard _) = [] 
boundVars (PBox _ p)    = boundVars p 

---------------------
--  Pretty printer --
---------------------

instance Pretty l => Pretty (Module l) where
  pretty (Module srcLocInfo moduleHead importDecl decl) = 
        nest 2 $ pretty "(Module" <> line 
    <+> pretty srcLocInfo <> line 
    <+> pretty moduleHead <> line 
    <+> pretty importDecl <> line 
    <+> pretty decl <> pretty ")"
instance Pretty l => Pretty (ModuleHead l) where
  pretty (ModuleHead srcLocInfo moduleName exportSpecList) = 
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
  -- pretty (IAbs srcLocInfo namespace name) = 
  --       nest 2 $ pretty "(IAbs" <> line 
  --   <+> pretty srcLocInfo <> line 
  --   <+> pretty namespace <> line 
  --   <+> pretty name <> pretty ")"

instance Pretty l => Pretty (Namespace l)  where
  pretty (NoNamespace srcLocInfo) = 
        pretty "(NoNamespace" <> line 
    <+> pretty srcLocInfo <> pretty ")"
  pretty (TypeNamespace srcLocInfo) = 
        pretty "(TypeNamespace" <> line 
    <+> pretty srcLocInfo <> pretty ")"
  pretty (PatternNamespace srcLocInfo) = 
        pretty "(PatternNamespace" <> line 
    <+> pretty srcLocInfo <> pretty ")"

instance Pretty l => Pretty (Decl l) where
  pretty (PatBind srcLocInfo pat rhd) = 
        nest 2 $ pretty "(PatBind" <> line 
    <+> pretty srcLocInfo <> line 
    <+> pretty pat <> line 
    <+> pretty rhd <> pretty ")"

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
  pretty (PBox srcLocInfo pat) = 
        nest 2 $ pretty "(PBox" <> line 
    <+> pretty srcLocInfo <> line
    <+> pretty pat <> pretty ")"

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
  pretty (If srcLocInfo exp1 exp2 exp3) = 
        nest 2 $ pretty "(If" <> line 
    <+> pretty srcLocInfo <> line 
    <+> pretty exp1 <> line 
    <+> pretty exp2 <> line 
    <+> pretty exp3  <> pretty ")"
  pretty (Lambda srcLocInfo pat exp) =
        nest 2 $ pretty "(Lambda" <> line 
    <+> pretty srcLocInfo <> line 
    <+> pretty pat <> line 
    <+> pretty exp <> pretty ")"
  pretty (Pr srcLocInfo exp) = 
        nest 2 $ pretty "(Pr" <> line 
    <+> pretty srcLocInfo <> line 
    <+> pretty exp <> pretty ")"
  -- pretty (LCase srcLocInfo alt) =
  --       nest 2 $ pretty "(LCase" <> line 
  --   <+> pretty srcLocInfo <> line 
  --   <+> pretty alt <> pretty ")"
  -- pretty _ = pretty "(Unknown)"