module Syntax.Type where

import Data.Map ( Map )
import Data.Set ( Set, union, isSubsetOf, empty )
import Data.List ( null )

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
