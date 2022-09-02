module Syntax.Type (
  ModuleName(..), QName(..), Name(..),
  Coeffect, Type(..), 
  Path(..), Version(..),
  freeTyVar, isClosed,
  coeff0, coeff1, zero, one,
  (.+), (.*), (.<),
  HasName(..)
) where

import Data.Map ( Map, toList )
import Data.Set ( Set, union, isSubsetOf, empty, toList )
import Data.List ( null )
import Util

import Syntax.Name (HasName(..))

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

instance HasName Name where
  getName (Ident str) = str
  getName (Symbol str) = str

instance HasName QName where
  getName (Qual _ name) = getName name
  getName (UnQual name) = getName name

instance HasName Type where
  getName (TyCon qName) = getName qName
  getName (TyVar name) = getName name
  getName _ = error "The getName function is not defined for a given expresion." 

type Coeffect = Type

-- | A type qualified with a context.
--   An unqualified type has an empty context.
data Type
  = TyCon QName         -- ^ named type or type constructor / Int
  | TyFun Type Type     -- ^ function type / A → B
  | TyVar Name          -- ^ type variable / α
  | TyBox Coeffect Type -- ^ promoted types / □_r A
  | TyBottom            -- ^ coeffect / coeffect-0
  | TyLabels (Set Path) -- ^ coeffect / fromList [](= coeffect-1), {l_1, l_2, ...}
  | CAdd Coeffect Coeffect -- ^ A constraint gnerated by (.+) when either of the coeffs is a type variable
  | CMul Coeffect Coeffect -- ^ A constraint gnerated by (.*) when either of the coeffs is a type variable
  deriving (Eq,Ord,Show)

newtype Path = Path (Map ModuleName Version)
  deriving (Eq,Ord,Show)

data Version = Version
  { major :: Int
  , minor :: Int
  , patch :: Int }
  deriving (Eq,Ord,Show)

coeff0 :: Coeffect
coeff0 = TyBottom

coeff1 :: Coeffect
coeff1 = TyLabels empty

zero :: Coeffect
zero = TyBottom

one :: Coeffect
one = TyLabels empty

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
  CAdd c1 c2  -> freeTyVar c1 ++ freeTyVar c2
  CMul c1 c2  -> freeTyVar c1 ++ freeTyVar c2

isClosed :: Type -> Bool
isClosed ty = Data.List.null $ freeTyVar ty

-- ^ Addition, multiplication, and partial-order in version resource semiring
(.+) :: Coeffect -> Coeffect -> Coeffect
(.+) TyBottom ty = ty
(.+) ty TyBottom = ty
(.+) (TyLabels s1) (TyLabels s2) = TyLabels $ union s1 s2
(.+) v@(TyVar _) c = CAdd v c
(.+) c v@(TyVar _) = CAdd c v
(.+) t1 t2 = error $ "The types " ++ show t1 ++ " and " ++ show t2 ++ " are not coeffect types."

(.*) :: Coeffect -> Coeffect -> Coeffect
(.*) TyBottom _ = TyBottom
(.*) _ TyBottom = TyBottom
(.*) (TyLabels s1) (TyLabels s2) = TyLabels $ union s1 s2
(.*) v@(TyVar _) c = CMul v c
(.*) c v@(TyVar _) = CMul c v
(.*) t1 t2 = error $ "The types " ++ show t1 ++ " and " ++ show t2 ++ " are not coeffect types."

(.<) :: Coeffect -> Coeffect -> Bool
(.<) TyBottom _ = True
(.<) _ TyBottom = False
(.<) (TyLabels s1) (TyLabels s2) = s1 `isSubsetOf` s2
(.<) t1 t2 = error $ "The types " ++ show t1 ++ " and " ++ show t2 ++ " are not coeffect types."

instance Pretty Type where
  pretty (TyCon qName) = nest 2 $ pretty "(TyCon" <+> pretty qName <> pretty ")"
  pretty (TyFun t1 t2) =
        nest 2 $ pretty "(TyFun" <> line
    <+> pretty t1 <> line
    <+> pretty t2 <> pretty ")"
  pretty (TyVar name)  = nest 2 $ pretty "(TyVar" <+> pretty name <> pretty ")"
  pretty (TyBox c ty)  =
        nest 2 $ pretty "(TyBox" <> line
    <+> pretty c <> line
    <+> pretty ty <> pretty ")"
  pretty TyBottom      = nest 2 $ pretty "(TyBottom)"
  pretty (TyLabels set_path) = 
    let pp_path = foldl (\acc p -> acc <+> pretty p) (pretty "") set_path in
    nest 2 $ pretty "(TyLabels" <> line
    <+> pp_path <> pretty ")"
  pretty (CAdd c1 c2) =
        nest 2 $ pretty "(CAdd" <> line
    <+> pretty c1 <> line
    <+> pretty c2 <> pretty ")"
  pretty (CMul c1 c2) =
        nest 2 $ pretty "(CMul" <> line
    <+> pretty c1 <> line
    <+> pretty c2 <> pretty ")"

instance Pretty Path where
  pretty (Path map) = nest 2 $ pretty "(Path" <+> pretty (Data.Map.toList map) <> pretty ")"

instance Pretty Version where
  pretty (Version major minor patch) =
    let pp_ver = pretty major <> pretty "." <> pretty minor <> pretty "." <> pretty patch in
    nest 2 $ pretty "(Version" <> line
    <+> pp_ver <> pretty ")"

instance Pretty QName where
  pretty (Qual modName qName) =
        nest 2 $ pretty "(Qual" <> line
    <+> pretty modName <> line
    <+> pretty qName <> pretty ")"
  pretty (UnQual name) = nest 2 $ pretty "(UnQual" <+> pretty name <> pretty ")"

instance Pretty Name where
  pretty (Ident str) = nest 2 $ pretty "(Ident" <+> pretty str <> pretty ")"
  pretty (Symbol str) = nest 2 $ pretty "(Symbol" <+> pretty str <> pretty ")"

instance Pretty ModuleName where
  pretty (ModuleName str) = nest 2 $ pretty "(ModuleName" <+> pretty str <> pretty ")"

-------------------------

instance PrettyAST Type where
  ppE = pretty
  ppP (TyCon qName) = ppP qName
  ppP (TyFun t1 t2) = parens $ ppP t1 <+> pretty "->" <+> ppP t2
  ppP (TyVar name)  = ppP name
  ppP (TyBox c ty)  = ppP ty <> pretty "@" <> brackets (ppP c)
  ppP TyBottom      = pretty "⊥" -- coeef 0
  ppP (TyLabels paths) = let p' = map ppP (Data.Set.toList paths) in
    case p' of
      [] -> pretty "{}" -- coeff 1
      _  -> list p'
  ppP (CAdd c1 c2) = ppP c1 <+> pretty ".+" <+> ppP c2
  ppP (CMul c1 c2) = ppP c1 <+> pretty ".*" <+> ppP c2

instance PrettyAST Path where
  ppE = pretty
  ppP (Path m) =
    let ppm = concatWith (surround comma) $ map (\(k,v) -> pretty k <+> pretty "->" <+> pretty v) (Data.Map.toList m)
    in parens $ pretty "Path" <+> ppm

instance PrettyAST Version where
  ppE = pretty
  ppP (Version major minor patch) = concatWith (surround dot) $ map pretty [major, minor, patch]

instance PrettyAST QName where
  ppE = pretty
  ppP (Qual modName qName) = ppP modName <> pretty "." <> ppP qName
  ppP (UnQual name) = ppP name

instance PrettyAST Name where
  ppE = pretty
  ppP (Ident str) = pretty str
  ppP (Symbol str) = pretty str

instance PrettyAST ModuleName where
  ppE = pretty
  ppP (ModuleName str) = pretty str