module Syntax.Env (
  EnvType(..),
  TEnv, UEnv, REnv(..),
  emptyTEnv, emptyUEnv, emptyREnv, basicTEnv,
  lookupTEnv, lookupUEnv,
  getType,
  (.++), (.**),
  gradeCtx,
  addTEnv, addUEnv,
  mulREnv,
  filterTEnv
) where

import Data.Map

import Syntax.Type
import Syntax.Kind

data EnvType
  = NType Type
  | GrType Type Coeffect
  deriving (Eq,Ord,Show)

getType :: EnvType -> Type
getType (NType t) = t
getType (GrType t _) = t

type TEnv = Map String EnvType
type UEnv = Map String Kind
data REnv = EmptyREnv | REnv Coeffect

emptyTEnv :: TEnv
emptyTEnv = empty

emptyUEnv :: UEnv
emptyUEnv = empty

emptyREnv :: REnv
emptyREnv = EmptyREnv

(.++) :: TEnv -> TEnv -> TEnv
(.++) = unionWith concat
  where
    concat :: EnvType -> EnvType -> EnvType
    concat t1 t2 = if getType t1 /= getType t2
      then
        let message = "type " ++ show t1 ++ " and " ++ show t2 ++ " is not equal" in
        error message
      else
        case (t1,t2) of
          (GrType t1 c1, GrType t2 c2) -> GrType t1 (c1 .+ c2)
          _ -> error $ "Both types " ++ show t1 ++ " and " ++ show t2 ++ " should be types with resources."

(.**) :: Coeffect -> TEnv -> TEnv
(.**) c = Data.Map.map (mul c)
  where
    mul :: Coeffect -> EnvType -> EnvType
    mul c ty@(NType t) = error $ "All variables in the TEnv must have version resources, but ty:" ++ show ty ++ " does not have a resource."
    mul c1 (GrType t c2) = GrType t (c1 .* c2)

gradeCtx :: TEnv -> TEnv
gradeCtx = Data.Map.map gradeTy
  where
    gradeTy :: EnvType -> EnvType
    gradeTy (NType t)       = GrType t one
    gradeTy ty@(GrType _ _) = ty

lookupTEnv :: String -> TEnv -> EnvType
lookupTEnv str tenv = case Data.Map.lookup str tenv of
  Nothing ->
    let message = "[lookupTEnv] The variable:" ++ show str ++ " is not in the tenv:" ++ show tenv ++ "." 
    in error message
  Just envty -> envty

lookupUEnv :: String -> UEnv -> Kind
lookupUEnv str uenv = case Data.Map.lookup str uenv of
  Nothing ->
    let message = "[lookupUEnv] The variable:" ++ show str ++ " is not in the uenv:" ++ show uenv ++ "." 
    in error message
  Just envty -> envty

addTEnv :: String -> EnvType -> TEnv -> TEnv
addTEnv = insert

addUEnv :: String -> Kind -> UEnv -> UEnv
addUEnv = insert

mulREnv :: Coeffect -> REnv -> REnv
mulREnv c EmptyREnv  = REnv c
mulREnv c1 (REnv c2) = REnv (c1 .* c2)

filterTEnv :: TEnv -> [String] -> TEnv
filterTEnv tenv keys = filterWithKey (\k _ -> k `elem` keys) tenv



intType :: Type
intType = TyCon (UnQual (Ident "Int"))

intToIntToInt :: Type
intToIntToInt = TyFun intType (TyFun intType intType)

basicTEnv :: TEnv
basicTEnv = fromList 
          [ ("+", NType intToIntToInt)
          , ("-", NType intToIntToInt)
          , ("*", NType intToIntToInt)
          , ("/", NType intToIntToInt) ]