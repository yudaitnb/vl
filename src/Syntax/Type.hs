module Syntax.Type (
  ModuleName(..), QName(..), Name(..),
  Coeffect, Type(..), 
  --Constraint(..), 
  Constraints(..), emptyConstraints, landC, lorC,
  isClosed,
  coeff0, coeff1,
  tyint, tybool, tychar, tystring,
  (.+), (.*), --(.<),
  HasName(..),
  tySym,
  sizeCs,
) where

import Data.Map ( Map, toList, union, unionWith, null )
import Data.Set ( Set, union, isSubsetOf, empty, toList, null, cartesianProduct, map )
import Data.List ( nub, null, map )

import Syntax.Common hiding (Name(..), QName (..), ModuleName(..))
import Util
import Graph

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
  getName ty = error "The getName function is not defined for a given expresion: " ++ putDocString (ppP ty)

type Coeffect = Type

-- | A type qualified with a context.
--   An unqualified type has an empty context.
data Type
  = TyCon QName         -- ^ named type or type constructor / Int
  | TyFun Type Type     -- ^ function type / A → B
  | TyVar Name          -- ^ type variable / α
  | TyBox Coeffect Type -- ^ promoted types / □_r A
  | TyBottom            -- ^ coeffect / coeffect-0
  | TyLabels Label -- ^ coeffect / fromList [](= coeffect-1), {l_1, l_2, ...}
  | TyTuple [Type]     -- ^ tuple type / (A, B, ...)
  | TyList Type       -- ^ list type / [A]
  | CAdd Coeffect Coeffect -- ^ A constraint gnerated by (.+) when either of the coeffs is a type variable
  | CMul Coeffect Coeffect -- ^ A constraint gnerated by (.*) when either of the coeffs is a type variable
  -- | CSubset Coeffect Coeffect -- ^ A constraint note that c1 ≤ c2
  deriving (Ord,Show)

-- type Constraint = Type
-- type Constraints = [Constraint]

-- landC :: Constraints -> Constraints -> Constraints
-- landC c1 c2 = c1 ++ c2 -- [TODO]

data Constraints
  = CSubset Coeffect Coeffect
  | CAnd Constraints Constraints
  | COr Constraints Constraints
  | CTop
  deriving (Eq, Show)

emptyConstraints :: Constraints
emptyConstraints = CTop

landC :: Constraints -> Constraints -> Constraints
landC CTop c2 = c2
landC c1 CTop = c1
landC c1 c2   = CAnd c1 c2

lorC :: Constraints -> Constraints -> Constraints
lorC CTop _ = CTop
lorC _ CTop = CTop
lorC c1 c2  = COr c1 c2

sizeCs :: Constraints -> Int
sizeCs cs = case cs of
  CTop        -> 0
  CSubset _ _ -> 1
  CAnd c1 c2  -> sizeCs c1 + sizeCs c2
  COr c1 c2   -> sizeCs c1 + sizeCs c2

instance Eq Type where
  TyVar n1 == TyVar n2 = getName n1 == getName n2
  TyCon qn == TyCon qn' = qn == qn'
  TyFun t1 t2 == TyFun t1' t2' = t1 == t1' && t2 == t2'
  TyBox c t == TyBox c' t' = c == c' && t == t'
  TyBottom == TyBottom = True
  TyLabels l == TyLabels l' = l == l'
  CAdd c1 c2 == CAdd c1' c2' = c1 == c1' && c2 == c2'
  CMul c1 c2 == CMul c1' c2' = c1 == c1' && c2 == c2'
  -- CSubset c1 c2 == CSubset c1' c2' = c1 == c1' && c2 == c2'
  t1 == t2 = False  

tySym :: Type -> Type -> Bool
tySym (TyVar _) (TyVar _) = True
tySym (TyCon qn) (TyCon qn') = qn == qn'
tySym (TyFun t1 t2) (TyFun t1' t2') = tySym t1 t1' && tySym t2 t2'
tySym (TyBox c t) (TyBox c' t') = tySym c c' && tySym t t'
tySym (TyTuple ts1) (TyTuple ts2) = foldr (\(t1, t2) acc -> tySym t1 t2 && acc) True (zip ts1 ts2)
tySym (TyList t1) (TyList t2) = tySym t1 t2
tySym TyBottom TyBottom = True
tySym (TyLabels l) (TyLabels l') = l == l'
tySym (CAdd c1 c2) (CAdd c1' c2') = tySym c1 c1' && tySym c2 c2'
tySym (CMul c1 c2) (CMul c1' c2') = tySym c1 c1' && tySym c2 c2'
-- tySym (CSubset c1 c2) (CSubset c1' c2') = tySym c1 c1' && tySym c2 c2'
tySym t1 t2 = False  

tyint :: Type
tyint = TyCon (UnQual (Ident "Int"))

tybool :: Type
tybool = TyCon (UnQual (Ident "Bool"))

tychar :: Type
tychar = TyCon (UnQual (Ident "Char"))

tystring :: Type
tystring = TyCon (UnQual (Ident "String"))

coeff0 :: Coeffect
coeff0 = TyBottom

coeff1 :: Coeffect
coeff1 = TyLabels emptyLabels

instance HasVar Type where
  freeVars ty = nub $ freeTyVars' ty
    where
      freeTyVars' ty =
        case ty of
        -- Types
        TyCon _     -> []
        TyVar name  -> [UQVar $ getName name]
        TyFun t1 t2 -> freeTyVars' t1 ++ freeTyVars' t2
        TyBox c t   -> freeTyVars' c ++ freeTyVars' t
        TyTuple ts  -> concatMap freeTyVars' ts
        TyList t    -> freeTyVars' t
        -- Coeffects
        TyBottom    -> []
        TyLabels _  -> []
        CAdd c1 c2  -> freeTyVars' c1 ++ freeTyVars' c2
        CMul c1 c2  -> freeTyVars' c1 ++ freeTyVars' c2
        -- Constraints
        -- CSubset c1 c2 -> freeTyVars' c1 ++ freeTyVars' c2
  vars  = freeVars

instance HasVar Constraints where
  freeVars cs = case cs of
    CTop          -> []
    CSubset c1 c2 -> nub $ freeVars c1 ++ freeVars c2
    CAnd c1 c2    -> nub $ freeVars c1 ++ freeVars c2
    COr c1 c2     -> nub $ freeVars c1 ++ freeVars c2
  vars  = freeVars

isClosed :: Type -> Bool
isClosed ty = Data.List.null $ freeVars ty

-- ^ Addition, multiplication, and partial-order in version resource semiring
(.+) :: Coeffect -> Coeffect -> Coeffect
(.+) TyBottom ty = ty
(.+) ty TyBottom = ty
(.+) t1@(TyLabels s1) t2
  | s1 == emptyLabels = t2
(.+) t1 t2@(TyLabels s2)
  | s2 == emptyLabels = t1
(.+) t1@(TyLabels labels1) t2@(TyLabels labels2) = --TyLabels $ Data.Set.map (\(Label a, Label b) -> Label $ Data.Map.union a b) $ cartesianProduct s1 s2 -- [TODO]
    TyLabels $ Data.Map.unionWith (++) labels1 labels2 -- [TODO]
(.+) c1 c2 = CAdd c1 c2
-- (.+) t1 t2 = error $ "The types " ++ show t1 ++ " and " ++ show t2 ++ " are not coeffect types."

(.*) :: Coeffect -> Coeffect -> Coeffect
(.*) TyBottom _ = TyBottom
(.*) _ TyBottom = TyBottom
(.*) t1@(TyLabels s1) t2
  | s1 == emptyLabels = t2
(.*) t1 t2@(TyLabels s2)
  | s2 == emptyLabels = t1
(.*) t1@(TyLabels labels1) t2@(TyLabels labels2) = --TyLabels $ Data.Set.map (\(Label a, Label b) -> Label $ Data.Map.union a b) $ cartesianProduct s1 s2 -- [TODO]
    TyLabels $ Data.Map.unionWith (++) labels1 labels2 -- [TODO]
(.*) c1 c2 = CMul c1 c2
-- (.*) t1 t2 = error $ "The types " ++ show t1 ++ " and " ++ show t2 ++ " are not coeffect types."

-- (.<) :: Coeffect -> Coeffect -> Bool
-- (.<) TyBottom _ = True
-- (.<) _ TyBottom = False
-- (.<) (TyLabels s1) (TyLabels s2) = s1 `isSubsetOf` s2
-- (.<) t1 t2 = error $ "The types " ++ show t1 ++ " and " ++ show t2 ++ " are not coeffect types."

-------------------------

instance PrettyAST Type where
  -- ^ Types
  ppP (TyCon qName) = ppP qName
  ppP (TyFun t1 t2) = parens $ ppP t1 <+> ppP "->" <+> ppP t2
  ppP (TyVar name)  = ppP name
  ppP (TyBox c ty)  = ppP ty <> ppP "@" <> brackets (ppP c)
  -- ^ Coeffects
  ppP TyBottom      = ppP "⊥" -- coeef 0
  ppP (TyTuple ts)  = parens $ ppP "Tuple" <+> concatWith (surround space) (Data.List.map ppP ts)
  ppP (TyList ty)   = parens $ ppP "List" <+> ppP ty
  ppP (TyLabels labels) = if Data.Map.null labels
    then ppP "{}" -- coeff 1
    else brackets $ ppP labels
  ppP (CAdd c1 c2) = ppP c1 <+> ppP ".+" <+> ppP c2
  ppP (CMul c1 c2) = ppP c1 <+> ppP ".*" <+> ppP c2
  -- ^ Constraints
  -- ppP (CSubset c1 c2) = ppP c1 <+> ppP "≤" <+> ppP c2

instance PrettyAST QName where
  ppP (Qual modName qName) = ppP modName <> ppP "." <> ppP qName
  ppP (UnQual name) = ppP name

instance PrettyAST Name where
  ppP (Ident str) = ppP str
  ppP (Symbol str) = ppP str

instance PrettyAST ModuleName where
  ppP (ModuleName str) = ppP str

instance PrettyAST Constraints where
  ppP cs = case cs of
    CTop -> ppP "⊤"
    CSubset c1 c2 -> parens $ ppP c1 <+> ppP "≤" <+> ppP c2
    CAnd c1 c2 -> parens $ ppP "and" <+> ppP c1 <+> ppP c2
    -- CAnd c1 c2 -> parens $ ppP c1 <+> ppP "and" <+> ppP c2
    COr c1 c2 -> parens $ ppP "or" <+> ppP c1 <+> ppP c2
    -- COr c1 c2 -> parens $ ppP c1 <+> ppP "or" <+> ppP c2