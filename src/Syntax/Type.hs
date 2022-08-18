module Syntax.Type where

import Data.Map ( Map )
import Data.List ( null )
import Foreign (free)

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
  = TyCon QName                  -- ^ named type or type constructor / Int
  | TyFun Type Type              -- ^ function type / A → B
  | TyVar Name                   -- ^ type variable / α
  | TyBox Coeffect Type          -- ^ promoted types / □_r A
  | TyBottom                     -- ^ coeffect / coeff0
  | TyLabels (Map Int Path)      -- ^ coeffect / {}(= coeff1), {l_1, l_2, ...}
  deriving (Eq,Ord,Show)

newtype Path = Path (Map ModuleName Version)
  deriving (Eq,Ord,Show)

data Version = Version
  { major :: Int
  , minor :: Int
  , patch :: Int }
  deriving (Eq,Ord,Show)

freeTyVar :: Type -> [String]
freeTyVar ty = case ty of
  TyCon _     -> []
  TyBottom    -> []
  TyLabels _  -> []
  TyVar name  -> [getName name]
  TyFun t1 t2 -> freeTyVar t1 ++ freeTyVar t2
  TyBox c t   -> freeTyVar c ++ freeTyVar t

isClosed :: Type -> Bool
isClosed ty = Data.List.null $ freeTyVar ty