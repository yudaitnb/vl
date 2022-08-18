module Syntax.LambdaVL where

import qualified Language.Haskell.Exts.Syntax as S
import Language.Haskell.Exts.SrcLoc
import Prettyprinter

class HasName a where
  getName :: a -> String

instance HasName (Name l) where
  getName (Ident _ str) = str
  getName (Symbol _ str) = str

instance HasName (QName l) where
  getName (Qual _ _ name) = getName name
  getName (UnQual _ name) = getName name

instance HasName (Pat l) where
  getName (PVar _ name) = getName name
  getName _             = "Patterns without PVar do not have a name field."

-- | A complete Haskell source module.
data Module l
    = Module l (Maybe (ModuleHead l)) [ImportDecl l] [Decl l]
    -- ^ an ordinary Haskell module
  deriving (Eq,Ord,Show)

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

-- | The name of a Haskell module.
data ModuleName l = ModuleName l String
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

-- | This type is used to represent qualified variables, and also
-- qualified constructors.
data QName l
    = Qual    l (ModuleName l) (Name l) -- ^ name qualified with a module name
    | UnQual  l                (Name l) -- ^ unqualified local name
  deriving (Eq,Ord,Show)

-- | This type is used to represent variables, and also constructors.
data Name l
    = Ident  l String   -- ^ /varid/ or /conid/.
    | Symbol l String   -- ^ /varsym/ or /consym/
  deriving (Eq,Ord,Show)

-- | A top-level declaration.
data Decl l
     = FunBind      l [Match l]
     -- ^ A set of function binding clauses
     | PatBind      l (Pat l) (Rhs l) {-where-} (Maybe (Binds l))
     -- ^ A pattern binding
  deriving (Eq,Ord,Show)

-- | The head of a type or class declaration, which consists of the type
-- or class name applied to some type variables
--
-- @class C a b@ is represented as
--
-- >DHApp
-- >   ()
-- >   (DHApp
-- >      () (DHead () (Ident () "C")) (UnkindedVar () (Ident () "a")))
-- >   (UnkindedVar () (Ident () "b"))
--
-- (where the annotation type @l@ is instantiated with @()@)
--
-- @class (a :< b) c@ is represented as
--
-- >DHApp
-- >   ()
-- >   (DHParen
-- >      ()
-- >      (DHApp
-- >         ()
-- >         (DHInfix () (UnkindedVar () (Ident () "a")) (Symbol () ":<"))
-- >         (UnkindedVar () (Ident () "b"))))
-- >   (UnkindedVar () (Ident () "c"))
data DeclHead l
    = DHead l (Name l) -- ^ type or class name
    | DHParen l (DeclHead l) -- ^ parenthesized declaration head
  deriving (Eq,Ord,Show)

-- | The right hand side of a function binding, pattern binding, or a case alternative.
data Rhs l
     = UnGuardedRhs l (Exp l) -- ^ unguarded right hand side (/exp/)
  deriving (Eq,Ord,Show)

-- | Clauses of a function binding.
data Match l
     = Match l      (Name l) [Pat l]         (Rhs l) {-where-} (Maybe (Binds l))
        -- ^ A clause defined with prefix notation, i.e. the function name
        --  followed by its argument patterns, the right-hand side and an
        --  optional where clause.
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

-- | Haskell expressions.
data Exp l
    = Var l (QName l)                       -- ^ variable
    | Lit l (Literal l)                     -- ^ literal constant
    | App l (Exp l) (Exp l)                 -- ^ ordinary application
    | NegApp l (Exp l)                      -- ^ negation expression @-/exp/@ (unary minus)
    | Lambda l [Pat l] (Exp l)              -- ^ lambda expression
    | Let l (Binds l) (Exp l)               -- ^ local declarations with @let@ ... @in@ ...
    | If l (Exp l) (Exp l) (Exp l)          -- ^ @if@ /exp/ @then@ /exp/ @else@ /exp/
    | InfixApp l (Exp l) (QOp l) (Exp l)    -- ^ infix application
    -- | Case l (Exp l) [Alt l]                -- ^ @case@ /exp/ @of@ /alts/
    -- | Paren l (Exp l)                       -- ^ parenthesised expression

-- Versioned expressions
    | Pr l (Exp l)                          -- ^ An expression promoted version resources

-- LambdaCase
    -- | LCase l [Alt l]                       -- ^ @\case@ /alts/

  deriving (Eq,Ord,Show)

-- | A binding group inside a @let@ or @where@ clause.
data Binds l
    = BDecls  l [Decl l]     -- ^ An ordinary binding group
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

-- | An /alt/ alternative in a @case@ expression.
data Alt l
    = Alt l (Pat l) (Rhs l) (Maybe (Binds l))
  deriving (Eq,Ord,Show)

-- | Possibly qualified infix operators (/qop/), appearing in expressions.
data QOp l
    = QVarOp l (QName l) -- ^ variable operator (/qvarop/)
    | QConOp l (QName l) -- ^ constructor operator (/qconop/)
  deriving (Eq,Ord,Show)

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
instance Pretty l => Pretty (ModuleName l) where
  pretty (ModuleName srcLocInfo str) = 
        nest 2 $ pretty "(ModuleName" <> line 
    <+> pretty srcLocInfo <> line 
    <+> pretty str <> pretty ")"
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

instance Pretty l => Pretty (QName l) where
  pretty (Qual srcLocInfo moduleName name) = 
        nest 2 $ pretty "(Qual" <> line 
    <+> pretty srcLocInfo <> line 
    <+> pretty moduleName <> line 
    <+> pretty name <> pretty ")"
  pretty (UnQual srcLocInfo name) = 
        nest 2 $ pretty "(UnQual" <> line 
    <+> pretty srcLocInfo <> line 
    <+> pretty name <> pretty ")"

instance Pretty l => Pretty (Name l) where
  pretty (Ident srcLocInfo string) = 
        nest 2 $ pretty "(Ident" <> line 
    <+> pretty srcLocInfo <> line 
    <+> pretty "\"" <> pretty string <> pretty "\"" <> pretty ")"
  pretty (Symbol srcLocInfo string) = 
        nest 2 $ pretty "(Symbol" <> line 
    <+> pretty srcLocInfo <> line 
    <+> pretty "\"" <> pretty string <> pretty "\"" <> pretty ")"

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

instance Pretty l => Pretty (Match l) where
  pretty (Match srcLocInfo name pat rhs binds) =
        nest 2 $ pretty "(Match" <> line 
    <+> pretty srcLocInfo <> line 
    <+> pretty name <> line 
    <+> pretty pat <> line 
    <+> pretty rhs <> line 
    <+> pretty binds <> pretty ")"

instance Pretty l => Pretty (Rhs l) where
  pretty (UnGuardedRhs srcLocInfo exp) = 
        nest 2 $ pretty "(UnGuardedRhs" <> line 
    <+> pretty srcLocInfo <> line 
    <+> pretty exp <> pretty ")"

instance Pretty l => Pretty (Binds l) where
  pretty (BDecls srcLocInfo decl) = 
        nest 2 $ pretty "(BDecls" <> line 
    <+> pretty srcLocInfo <> line 
    <+> pretty decl <> pretty ")"

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
  pretty (Lambda srcLocInfo pat exp) =
        nest 2 $ pretty "(Lambda" <> line 
    <+> pretty srcLocInfo <> line 
    <+> pretty pat <> line 
    <+> pretty exp <> pretty ")"
  pretty (InfixApp srcLocInfo exp1 qOp exp2) =
        nest 2 $ pretty "(InfixApp" <> line 
    <+> pretty srcLocInfo <> line 
    <+> pretty exp1 <> line 
    <+> pretty qOp <> line 
    <+> pretty exp2 <> pretty ")"
  -- pretty (LCase srcLocInfo alt) =
  --       nest 2 $ pretty "(LCase" <> line 
  --   <+> pretty srcLocInfo <> line 
  --   <+> pretty alt <> pretty ")"
  pretty (Pr srcLocInfo exp) = 
        nest 2 $ pretty "(Pr" <> line 
    <+> pretty srcLocInfo <> line 
    <+> pretty exp <> pretty ")"
  -- pretty _ = pretty "(Unknown)"

instance Pretty l => Pretty (QOp l) where
  pretty (QVarOp srcLocInfo qName) =
        nest 2 $ pretty "(QVarOp" <> line 
    <+> pretty srcLocInfo <> line 
    <+> pretty qName <> pretty ")"
  pretty (QConOp srcLocInfo qName) =
        nest 2 $ pretty "(QConOp" <> line 
    <+> pretty srcLocInfo <> line 
    <+> pretty qName <> pretty ")"