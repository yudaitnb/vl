{-# LANGUAGE FlexibleInstances #-}
module Syntax.Type (
  ModuleName(..), QName(..), Name(..),
  Coeffect, Type(..), Constraint(..), Constraints(..), emptyConstraints, landC,
  Path(..), Version(..),
  freeTyVars, isClosed,
  coeff0, coeff1, zero, one,
  (.+), (.*), (.<),
  HasName(..)
) where

import Data.Map ( Map, toList )
import Data.Set ( Set, union, isSubsetOf, empty, toList )
import Data.List ( null, nub )
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
type Constraint = Type
newtype Constraints = Constraints { constraints :: [Constraint] }
  deriving (Eq, Ord, Show)

emptyConstraints :: Constraints
emptyConstraints = Constraints []

landC :: Constraints -> Constraints -> Constraints
landC c1 c2 = Constraints $ constraints c1 ++ constraints c2

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
  | CSubset Coeffect Coeffect -- ^ A constraint note that c1 ≤ c2
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

freeTyVars :: Type -> [String]
freeTyVars ty = nub $ freeTyVars' ty
  where
    freeTyVars' ty =
      case ty of
      -- Types
      TyCon _     -> []
      TyVar name  -> [getName name]
      TyFun t1 t2 -> freeTyVars' t1 ++ freeTyVars' t2
      TyBox c t   -> freeTyVars' c ++ freeTyVars' t
      -- Coeffects
      TyBottom    -> []
      TyLabels _  -> []
      CAdd c1 c2  -> freeTyVars' c1 ++ freeTyVars' c2
      CMul c1 c2  -> freeTyVars' c1 ++ freeTyVars' c2
      -- Constraints
      CSubset c1 c2 -> freeTyVars' c1 ++ freeTyVars' c2

isClosed :: Type -> Bool
isClosed ty = Data.List.null $ freeTyVars ty

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

-------------------------

instance PrettyAST Type where
  ppE (TyCon qName) = nest 2 $ ppE "(TyCon" <+> ppE qName <> ppE ")"
  ppE (TyFun t1 t2) =
        nest 2 $ ppE "(TyFun" <> line
    <+> ppE t1 <> line
    <+> ppE t2 <> ppE ")"
  ppE (TyVar name)  = nest 2 $ ppE "(TyVar" <+> ppE name <> ppE ")"
  ppE (TyBox c ty)  =
        nest 2 $ ppE "(TyBox" <> line
    <+> ppE c <> line
    <+> ppE ty <> ppE ")"
  ppE TyBottom      = nest 2 $ ppE "(TyBottom)"
  ppE (TyLabels set_path) =
    let pp_path = foldl (\acc p -> acc <+> ppE p) (ppE "") set_path in
    nest 2 $ ppE "(TyLabels" <> line
    <+> pp_path <> ppE ")"
  ppE (CAdd c1 c2) =
        nest 2 $ ppE "(CAdd" <> line
    <+> ppE c1 <> line
    <+> ppE c2 <> ppE ")"
  ppE (CMul c1 c2) =
        nest 2 $ ppE "(CMul" <> line
    <+> ppE c1 <> line
    <+> ppE c2 <> ppE ")"
  ppE (CSubset c1 c2) =
        nest 2 $ ppE "(CSubset" <> line
    <+> ppE c1 <> line
    <+> ppE c2 <> ppE ")"
  -- ^ Types
  ppP (TyCon qName) = ppP qName
  ppP (TyFun t1 t2) = parens $ ppP t1 <+> ppP "->" <+> ppP t2
  ppP (TyVar name)  = ppP name
  ppP (TyBox c ty)  = ppP ty <> ppP "@" <> brackets (ppP c)
  -- ^ Coeffects
  ppP TyBottom      = ppP "⊥" -- coeef 0
  ppP (TyLabels paths) = let p' = map ppP (Data.Set.toList paths) in
    case p' of
      [] -> ppP "{}" -- coeff 1
      _  -> list p'
  ppP (CAdd c1 c2) = ppP c1 <+> ppP ".+" <+> ppP c2
  ppP (CMul c1 c2) = ppP c1 <+> ppP ".*" <+> ppP c2
  -- ^ Constraints
  ppP (CSubset c1 c2) = ppP c1 <+> ppP "≤" <+> ppP c2

instance PrettyAST Path where
  ppE (Path m) = nest 2 $ ppE "(Path" <+> pplist ppE (Data.Map.toList m) <> ppE ")"
  ppP (Path m) =
    let ppm = concatWith (surround comma) $ map (\(k,v) -> ppP k <+> ppP "->" <+> ppP v) (Data.Map.toList m)
    in parens $ ppP "Path" <+> ppm

instance PrettyAST Version where
  ppE (Version major minor patch) =
    let pp_ver = ppE major <> ppE "." <> ppE minor <> ppE "." <> ppE patch in
    nest 2 $ parens $ ppE "Version" <> line
    <+> pp_ver
  ppP (Version major minor patch) = concatWith (surround dot) $ map ppP [major, minor, patch]

instance PrettyAST QName where
  ppE (Qual modName qName) =
        nest 2 $ parens $ ppE "Qual" <> line
    <+> ppE modName <> line
    <+> ppE qName
  ppE (UnQual name) = nest 2 $ parens $ ppE "UnQual" <+> ppE name
  ppP (Qual modName qName) = ppP modName <> ppP "." <> ppP qName
  ppP (UnQual name) = ppP name

instance PrettyAST Name where
  ppE (Ident str) = nest 2 $ parens $ ppE "Ident" <+> ppE str
  ppE (Symbol str) = nest 2 $ parens $ ppE "Symbol" <+> ppE str
  ppP (Ident str) = ppP str
  ppP (Symbol str) = ppP str

instance PrettyAST ModuleName where
  ppE (ModuleName str) = nest 2 $ parens $ ppE "ModuleName" <+> ppE str
  ppP (ModuleName str) = ppP str

instance PrettyAST Constraints where
  ppE (Constraints []) = ppE "⊤" -- [TODO]
  ppE (Constraints cs) = concatWith (surround $ ppE "∧") $ map ppP cs
  ppP (Constraints []) = ppP "⊤"
  ppP (Constraints cs) = concatWith (surround $ ppP "∧") $ map ppP cs