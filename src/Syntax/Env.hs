{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Syntax.Env (
  Env, Env'(..),
  Environment(..),
  EnvType(..), getType,
  getLogs, setLogs, initLogs, putLog,
  getCounter, setCounter, initCounter,
  TEnv, getTEnv, setTEnv, initTEnv,
  UEnv, getUEnv, setUEnv, initUEnv,
  REnv(..),getREnv, setREnv, initREnv, mulREnv, emptyREnv,
  initEnv,
  (.++), (.**),
  gradeTEnv,
  genNewTyVar,
  basicType, basicTEnv,
  --Logs
  Logs, showTyInfEnv, showPatSynthEnv, showKindEnv
) where

import Data.Map

import Prelude hiding (lookup)
import Syntax.Type
import Syntax.Kind
import Control.Monad.State
import Util

prefixNewTyVar :: String
prefixNewTyVar = "a"

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
  deriving (Show)

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
  emptyEnv = empty
  makeEnv = fromList
  insertEnv = insert
  lookupEnv = lookup
  filterEnvBy keys = filterWithKey (\k _ -> k `elem` keys)
  hasBinding k v tenv =
    case lookupEnv k tenv of
      Nothing  -> False
      Just res -> res == v
  exclude = difference

instance Environment UEnv where
  type Key UEnv = String
  type Value UEnv = Kind
  emptyEnv = empty
  makeEnv = fromList
  insertEnv = insert
  lookupEnv = lookup
  filterEnvBy keys = filterWithKey (\k _ -> k `elem` keys)
  hasBinding k v tenv =
    case lookupEnv k tenv of
      Nothing  -> False
      Just res -> res == v
  exclude = difference

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

gradeTEnv :: TEnv -> TEnv
gradeTEnv = Data.Map.map gradeTy
  where
    gradeTy :: EnvType -> EnvType
    gradeTy (NType t)       = GrType t one
    gradeTy ty@(GrType _ _) = ty

data Env' = Env'
  { counter :: Int -- 単一化型変数の累積数
  , tEnv :: TEnv   -- 型付け環境 / [x:A]
  , uEnv :: UEnv   -- 単一化用型変数環境 / [X:κ]
  , rEnv :: REnv   -- リソース環境 / -, r
  , logs :: Logs
  }
  deriving (Show)
type Env a = State Env' a

type Logs = [String]

-- ^ Logs
getLogs :: Env Logs
getLogs = state $ \env@(Env' _ _ _ _ l) -> (l, env)

setLogs :: Logs -> Env ()
setLogs logs = state $ \(Env' c t u r _) -> ((), Env' c t u r logs)

initLogs :: Env ()
initLogs = state $ \(Env' c t u r _) -> ((), Env' c t u r [])

putLog :: String -> Env ()
putLog str = state $ \env@(Env' c t u r l) -> ((), Env' c t u r (str:l))

-- ^ Counter
getCounter :: Env Int
getCounter = state $ \env@(Env' c _ _ _ _) -> (c, env)

setCounter :: Int -> Env ()
setCounter n = state $ \(Env' _ t u r l) -> ((), Env' n t u r l)

initCounter :: Env ()
initCounter = setCounter 0

-- ^ TEnv
getTEnv :: Env TEnv
getTEnv = state $ \env@(Env' _ t _ _ _) -> (t, env)

setTEnv :: TEnv -> Env ()
setTEnv tenv = state $ \(Env' c _ u r l) -> ((), Env' c tenv u r l)

initTEnv :: Env ()
initTEnv = setTEnv emptyEnv

-- ^ UEnv
getUEnv :: Env UEnv
getUEnv = state $ \env@(Env' _ _ u _ _) -> (u, env)

setUEnv :: UEnv -> Env ()
setUEnv uenv = state $ \(Env' c t _ r l) -> ((), Env' c t uenv r l)

initUEnv :: Env ()
initUEnv = setUEnv emptyEnv

-- ^ REnv
emptyREnv :: REnv
emptyREnv = EmptyREnv

getREnv :: Env REnv
getREnv = state $ \env@(Env' _ _ _ r _) -> (r, env)

setREnv :: REnv -> Env ()
setREnv renv = state $ \(Env' c t u _ l) -> ((), Env' c t u renv l)

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
genNewTyVar kind = state $ \(Env' c t u r l) ->
  let name = prefixNewTyVar ++ show c
      c' = c + 1
      uenv' = insertEnv name kind u
      tyvar = TyVar (Ident name)
  in (tyvar, Env' c' t uenv' r l)

---------------------------------------

basicType :: [String]
basicType = ["Int", "String", "Char"]

intTy :: Type
intTy = TyCon (UnQual (Ident "Int"))

bxIntTy1 :: Type
bxIntTy1 =
  TyBox
    (TyVar (Ident "a10"))
    (TyCon (UnQual (Ident "Int")))

bxIntTy2 :: Type
bxIntTy2 =
  TyBox
    (TyVar (Ident "a11"))
    (TyCon (UnQual (Ident "Int")))

intToInt :: Type
intToInt = TyFun bxIntTy1 intTy

intToIntToInt :: Type
intToIntToInt = TyFun bxIntTy2 intToInt

basicTEnv :: TEnv
basicTEnv = fromList []
          -- [ ("+", NType intToIntToInt)
          -- , ("-", NType intToIntToInt)
          -- , ("*", NType intToIntToInt)
          -- , ("/", NType intToIntToInt) ]

-----------------------------

instance Pretty EnvType where
  pretty (NType ty) = pretty ty
  pretty (GrType c ty) = brackets (pretty ty) <> pretty "_" <> parens (pretty c)

instance Pretty TEnv where
  pretty = foldlWithKey
    (\acc k v -> acc <+>
      pretty k <> comma <> pretty v)
    emptyDoc

instance Pretty UEnv where
  pretty = foldlWithKey
    (\acc k v -> acc <+>
      pretty k <> comma <> pretty v)
    emptyDoc

instance Pretty REnv where
  pretty EmptyREnv = pretty "-"
  pretty (REnv c) = pretty c

------------------------------

instance PrettyAST EnvType where
  ppE = pretty
  ppP (NType ty) = ppP ty
  ppP (GrType ty c) = brackets (ppP ty) <> pretty "_" <> parens (ppP c)

instance PrettyAST TEnv where
  ppE = pretty
  ppP m
    | m == empty = emptyset
    | otherwise  = concatWith (surround $ comma <> space) $ Prelude.map (\(k,v) -> pretty k <> colon <> ppP v) (toList m)

instance PrettyAST UEnv where
  ppE = pretty
  ppP m
    | m == empty = emptyset
    | otherwise  = concatWith (surround $ comma <> space) $ Prelude.map (\(k,v) -> pretty k <> colon <> ppP v) (toList m)

instance PrettyAST REnv where
  ppE = pretty
  ppP = pretty

instance PrettyAST Logs where
  ppE = pretty
  ppP logs = vsep $ Prelude.map pretty logs

------------------------------

showTyInfEnv :: Env String
showTyInfEnv = do
  t <- getTEnv
  u <- getUEnv
  let doc = ppP u <> semicolon <+> ppP t <> vdash
  return $ putDocString doc

showPatSynthEnv :: Env String
showPatSynthEnv = do
  u <- getUEnv
  r <- getREnv
  let doc = ppP u <> semicolon <+> ppP r <> vdash
  return $ putDocString doc

showKindEnv :: Env String
showKindEnv = do
  u <- getUEnv
  let doc = ppP u <> vdash
  return $ putDocString doc