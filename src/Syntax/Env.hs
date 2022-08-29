{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Syntax.Env (
  Env, Env'(..),
  Environment(..),
  EnvType(..), getType,
  getCounter, setCounter, initCounter,
  TEnv, getTEnv, setTEnv, initTEnv,
  UEnv, getUEnv, setUEnv, initUEnv,
  REnv(..),getREnv, setREnv, initREnv, mulREnv, emptyREnv,
  initEnv,
  (.++), (.**),
  gradeTEnv,
  genNewTyVar,
  basicType, basicTEnv
) where

import qualified Data.Map as M

import Syntax.Type
import Syntax.Kind
import Control.Monad.State

data EnvType
  = NType Type
  | GrType Type Coeffect
  deriving (Eq,Ord,Show)

getType :: EnvType -> Type
getType (NType t) = t
getType (GrType t _) = t

type TEnv = M.Map String EnvType
type UEnv = M.Map String Kind
data REnv = EmptyREnv | REnv Coeffect

class Environment env where
  type Key env
  type Value env
  emptyEnv :: env
  makeEnv :: [(Key env, Value env)] -> env
  lookupEnv :: Key env -> env -> Maybe (Value env)
  insertEnv :: Key env -> Value env -> env -> env
  filterEnvBy :: [Key env] -> env -> env
  hasBinding :: Key env -> Value env -> env -> Bool
  exclude :: env -> env -> env

instance Environment TEnv where
  type Key TEnv = String
  type Value TEnv = EnvType
  emptyEnv = M.empty
  makeEnv = M.fromList
  insertEnv = M.insert
  lookupEnv = M.lookup
  filterEnvBy keys = M.filterWithKey (\k _ -> k `elem` keys)
  hasBinding k v tenv =
    case lookupEnv k tenv of
      Nothing  -> False
      Just res -> res == v
  exclude = M.difference

instance Environment UEnv where
  type Key UEnv = String
  type Value UEnv = Kind
  emptyEnv = M.empty
  makeEnv = M.fromList
  insertEnv = M.insert
  lookupEnv = M.lookup
  filterEnvBy keys = M.filterWithKey (\k _ -> k `elem` keys)
  hasBinding k v tenv =
    case lookupEnv k tenv of
      Nothing  -> False
      Just res -> res == v
  exclude = M.difference

(.++) :: TEnv -> TEnv -> TEnv
(.++) = M.unionWith concat
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
(.**) c = M.map (mul c)
  where
    mul :: Coeffect -> EnvType -> EnvType
    mul c ty@(NType t) = error $ "All variables in the TEnv must have version resources, but ty:" ++ show ty ++ " does not have a resource."
    mul c1 (GrType t c2) = GrType t (c1 .* c2)

gradeTEnv :: TEnv -> TEnv
gradeTEnv = M.map gradeTy
  where
    gradeTy :: EnvType -> EnvType
    gradeTy (NType t)       = GrType t one
    gradeTy ty@(GrType _ _) = ty

data Env' = Env'
  { counter :: Int -- 単一化型変数の累積数
  , tEnv :: TEnv   -- 型付け環境 / [x:A]
  , uEnv :: UEnv   -- 単一化用型変数環境 / [X:κ]
  , rEnv :: REnv   -- リソース環境 / 
  }
type Env a = State Env' a

prefixNewTyVar :: String
prefixNewTyVar = "_tyvar_"

-- ^ Counter
getCounter :: Env Int
getCounter = state $ \env@(Env' c _ _ _) -> (c, env)

setCounter :: Int -> Env ()
setCounter n = state $ \(Env' _ t u r) -> ((), Env' n t u r)

initCounter :: Env ()
initCounter = setCounter 0

-- ^ TEnv
getTEnv :: Env TEnv
getTEnv = state $ \env@(Env' _ t _ _) -> (t, env)

setTEnv :: TEnv -> Env ()
setTEnv tenv = state $ \(Env' c _ u r) -> ((), Env' c tenv u r)

initTEnv :: Env ()
initTEnv = setTEnv emptyEnv

-- ^ UEnv
getUEnv :: Env UEnv
getUEnv = state $ \env@(Env' _ _ u _) -> (u, env)

setUEnv :: UEnv -> Env ()
setUEnv uenv = state $ \(Env' c t _ r) -> ((), Env' c t uenv r)

initUEnv :: Env ()
initUEnv = setUEnv emptyEnv

-- ^ REnv
emptyREnv :: REnv
emptyREnv = EmptyREnv

getREnv :: Env REnv
getREnv = state $ \env@(Env' _ _ _ r) -> (r, env)

setREnv :: REnv -> Env ()
setREnv renv = state $ \(Env' c t u _) -> ((), Env' c t u renv)

initREnv :: Env ()
initREnv = setREnv emptyREnv

mulREnv :: Coeffect -> REnv -> REnv
mulREnv c EmptyREnv  = REnv c
mulREnv c1 (REnv c2) = REnv (c1 .* c2)

-- ^ initialize all environment
initEnv :: Env ()
initEnv = do
  initCounter
  initTEnv
  initUEnv
  initREnv

-- ^ Generate new type variable
--   counter++, return a generated type variable and new environment
genNewTyVar :: Kind -> Env Type
genNewTyVar kind = state $ \(Env' c t u r) ->
  let name = prefixNewTyVar ++ show c
      c' = c + 1
      uenv' = insertEnv name kind u
      tyvar = TyVar (Ident name)
  in (tyvar, Env' c' t uenv' r)

---------------------------------------

basicType :: [String]
basicType = ["Int", "String", "Char"]

intType :: Type
intType = TyCon (UnQual (Ident "Int"))

intToIntToInt :: Type
intToIntToInt = TyFun intType (TyFun intType intType)

basicTEnv :: TEnv
basicTEnv = M.fromList
          [ ("+", NType intToIntToInt)
          , ("-", NType intToIntToInt)
          , ("*", NType intToIntToInt)
          , ("/", NType intToIntToInt) ]