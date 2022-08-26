module Syntax.Type where

import Data.Map ( Map, toList )
import Data.Set ( Set, union, isSubsetOf, empty )
import Data.List ( null )
import Prettyprinter

newtype ModuleName
  = ModuleName String
  deriving (Eq,Ord,Show)

data QName
  = Qual    ModuleName Name -- ^ name qualified with a module name
  | UnQual             Name -- ^ unqualified local name
  deriving (Eq,Ord,Show)

data Name
  = Ident  String   -- ^ /varid/ or /conid/.
  | Symbol String   -- ^ /varsym/ or /consym/
  deriving (Eq,Ord,Show)

class HasName a where
  getName :: a -> String

instance HasName Name where
  getName (Ident str) = str
  getName (Symbol str) = str

instance HasName QName where
  getName (Qual _ name) = getName name
  getName (UnQual name) = getName name

type Coeffect = Type

-- | A type qualified with a context.
--   An unqualified type has an empty context.
data Type
  = TyCon QName          -- ^ named type or type constructor / Int
  | TyFun Type Type      -- ^ function type / A → B
  | TyVar Name           -- ^ type variable / α
  | TyBox Coeffect Type  -- ^ promoted types / □_r A
  | TyBottom             -- ^ coeffect / coeffect-0
  | TyLabels (Set Path)  -- ^ coeffect / fromList [](= coeffect-1), {l_1, l_2, ...}
  deriving (Eq,Ord,Show)

coeff0 :: Coeffect
coeff0 = TyBottom

coeff1 :: Coeffect
coeff1 = TyLabels empty

zero :: Coeffect
zero = TyBottom

one :: Coeffect
one = TyLabels empty

newtype Path = Path (Map ModuleName Version)
  deriving (Eq,Ord,Show)

data Version = Version
  { major :: Int
  , minor :: Int
  , patch :: Int }
  deriving (Eq,Ord,Show)

freeTyVar :: Type -> [String]
freeTyVar ty = case ty of
  -- Types
  TyCon _     -> []
  TyVar name  -> [getName name]
  TyFun t1 t2 -> freeTyVar t1 ++ freeTyVar t2
  TyBox c t   -> freeTyVar c ++ freeTyVar t
  -- Coeffects
  TyBottom    -> []
  TyLabels _  -> []

isClosed :: Type -> Bool
isClosed ty = Data.List.null $ freeTyVar ty

-- ^ Addition, multiplication, and partial-order in version resource semiring
(.+) :: Coeffect -> Coeffect -> Coeffect
(.+) TyBottom ty = ty
(.+) ty TyBottom = ty
(.+) (TyLabels s1) (TyLabels s2) = TyLabels $ union s1 s2
(.+) t1 t2 = error $ "The types " ++ show t1 ++ " and " ++ show t2 ++ " are not coeffect types."

(.*) :: Coeffect -> Coeffect -> Coeffect
(.*) TyBottom _ = TyBottom
(.*) _ TyBottom = TyBottom
(.*) (TyLabels s1) (TyLabels s2) = TyLabels $ union s1 s2
(.*) t1 t2 = error $ "The types " ++ show t1 ++ " and " ++ show t2 ++ " are not coeffect types."

(.<) :: Coeffect -> Coeffect -> Bool
(.<) TyBottom _ = True
(.<) _ TyBottom = False
(.<) (TyLabels s1) (TyLabels s2) = s1 `isSubsetOf` s2
(.<) t1 t2 = error $ "The types " ++ show t1 ++ " and " ++ show t2 ++ " are not coeffect types."

instance Pretty Type where
  -- pretty (TyCon qName) = nest 2 $ pretty "(TyCon" <+> pretty qName <> pretty ")"
  -- pretty (TyFun t1 t2) =
  --       nest 2 $ pretty "(TyFun" <> line
  --   <+> pretty t1 <> line
  --   <+> pretty t2 <> pretty ")"
  -- pretty (TyVar name)  = nest 2 $ pretty "(TyVar" <+> pretty name <> pretty ")"
  -- pretty (TyBox c ty)  =
  --       nest 2 $ pretty "(TyBox" <> line
  --   <+> pretty c <> line
  --   <+> pretty ty <> pretty ")"
  -- pretty TyBottom      = nest 2 $ pretty "(TyBottom)"
  -- pretty (TyLabels set_path) = 
  --   let pp_path = foldl (\acc p -> acc <+> pretty p) (pretty "") set_path in
  --   nest 2 $ pretty "(TyLabels" <> line
  --   <+> pp_path <> pretty ")"
  pretty (TyCon qName) = nest 2 $ pretty qName
  pretty (TyFun t1 t2) = nest 2 $ pretty t1 <> pretty " -> " <> pretty t2
  pretty (TyVar name)  = nest 2 $ pretty name
  pretty (TyBox c ty)  = nest 2 $ pretty ty <> pretty "@" <> brackets (pretty c)
  pretty TyBottom      = nest 2 $ pretty "⊥"
  pretty (TyLabels set_path) = 
    let pp_path = foldl (\acc p -> acc <+> pretty p) (pretty "") set_path in
    nest 2 pp_path

instance Pretty Path where
  -- pretty (Path map) = nest 2 $ pretty "(Path" <+> pretty (toList map) <> pretty ")"
  pretty (Path map) = nest 2 $ pretty (toList map)

instance Pretty Version where
  pretty (Version major minor patch) =
    let pp_ver = pretty major <> pretty "." <> pretty minor <> pretty "." <> pretty patch in
    nest 2 $ pretty "(Version" <> line
    <+> pp_ver <> pretty ")"

instance Pretty QName where
  -- pretty (Qual modName qName) =
  --       nest 2 $ pretty "(Qual" <> line
  --   <+> pretty modName <> line
  --   <+> pretty qName <> pretty ")"
  -- pretty (UnQual name) = nest 2 $ pretty "(UnQual" <+> pretty name <> pretty ")"
  pretty (Qual modName qName) = nest 2 $ pretty modName <> pretty "." <> pretty qName
  pretty (UnQual name) = nest 2 $ pretty name

instance Pretty Name where
  -- pretty (Ident str) = nest 2 $ pretty "(Ident" <+> pretty str <> pretty ")"
  -- pretty (Symbol str) = nest 2 $ pretty "(Symbol" <+> pretty str <> pretty ")"
  pretty (Ident str) = nest 2 $ pretty str
  pretty (Symbol str) = nest 2 $ pretty str

instance Pretty ModuleName where
  -- pretty (ModuleName str) = nest 2 $ pretty "(ModuleName" <+> pretty str <> pretty ")"
  pretty (ModuleName str) = nest 2 $ pretty str