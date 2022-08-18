module Syntax.Env where

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

contextConcat :: TEnv -> TEnv -> TEnv
contextConcat tenv1 tenv2 = unionWith concat tenv1 tenv2
  where
    concat :: EnvType -> EnvType -> EnvType
    concat t1 t2 = if (getType t1) != (getType t2)
      then
        let message = "type " ++ show t1 ++ " and " ++ show t2 ++ " is not equal"

contextGrading :: TEnv -> TEnv
contextGrading = Data.Map.map grade
  where
    grade :: EnvType -> EnvType
    grade (NType t)       = GrType t (TyLabels empty)
    grade ty@(GrType _ _) = ty