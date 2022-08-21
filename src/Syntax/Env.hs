module Syntax.Env (
  EnvType(..),
  TEnv, UEnv, REnv,
  emptyTEnv, emptyUEnv, emptyREnv,
  (!),
  getType,
  (.++), (.**),
  gradeCtx
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
data REnv = Empty | REnv Coeffect

emptyTEnv :: TEnv
emptyTEnv = empty

emptyUEnv :: UEnv
emptyUEnv = empty

emptyREnv :: REnv
emptyREnv = Empty

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

mulREnv :: Coeffect -> REnv -> REnv
mulREnv c Empty      = REnv c
mulREnv c1 (REnv c2) = REnv (c1 .* c2)