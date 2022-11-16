module Syntax.Env (
  Env, Env'(..),
  Environment(..),
  EnvType(..), getType,
  setCounter, initCounter, initializeCounter,
  TEnv, setTEnv, initializeTEnv, putTEnv,
  UEnv, setUEnv, initializeUEnv,
  REnv(..), setREnv, initializeREnv, mulREnv, emptyREnv,
  ExVarResources(..),
  -- setCEnv, initializeCEnv, putCEnv,
  setLogs, initializeLogs, putLog, debugE, debugP, debug,
  addLC, subLC, initializeLC, initLC,
  initializeEnv, initEnv,
  (.++), (.++.), (.+++), (.**),
  gradeTEnv,
  genNewTyVar,
  basicTypes,
  genConstraint, genConstraintBy, genConstraintPull,
  --Logs
  Logs, reverseLogs, emptyLogs,
  prefixNewTyVar,
  Tag(..)
) where

import Prelude hiding (log, lookup)
import Control.Monad.State

import qualified Data.List
import Data.Map

import Syntax.Type as T
import Syntax.Kind
import Syntax.Common (HasVar(..), VarKey(..), VarName(..), ModName(..), SrcSpanInfo(..), mkQual, mkUnQual)
import Syntax.Common as N (QName(..))

import Parser (VLMod(..))

import Util

prefixNewTyVar :: String
prefixNewTyVar = "a"

data EnvType
  = NType Tag Type
  | GrType Tag Type Coeffect
  deriving (Eq,Ord,Show)

data Tag = Imported | Local deriving (Eq, Ord, Show)

getType :: EnvType -> Type
getType (NType _ t) = t
getType (GrType _ t _) = t

type TEnv = Map VarKey EnvType
-- type TEnv = Map VarName EnvType
type UEnv = Map VarName Kind
data REnv = EmptyREnv | REnv Coeffect deriving (Show)
type ExVarResources = Map VarKey (VarKey, Type) -- 元の名前, 付与されたリソース変数

class Environment env where
  type Key env
  type Value env
  emptyEnv :: env
  lookupEnv :: Key env -> env -> Maybe (Value env)
  insertEnv :: Key env -> Value env -> env -> env
  filterEnvBy :: [Key env] -> env -> env
  hasBinding :: Key env -> Value env -> env -> Bool
  exclude :: env -> env -> env
  varsInEnv :: env -> [Key env]

instance Environment TEnv where
  type Key TEnv = VarKey
  type Value TEnv = EnvType
  emptyEnv = empty
  insertEnv = insert
  lookupEnv = lookup
  filterEnvBy keys = filterWithKey (\k _ -> k `elem` keys)
  hasBinding k v tenv =
    case lookupEnv k tenv of
      Nothing  -> False
      Just res -> res == v
  exclude = difference
  varsInEnv = keys

instance Environment UEnv where
  type Key UEnv = VarName
  type Value UEnv = Kind
  emptyEnv = empty
  insertEnv = insert
  lookupEnv = lookup
  filterEnvBy keys = filterWithKey (\k _ -> k `elem` keys)
  hasBinding k v tenv =
    case lookupEnv k tenv of
      Nothing  -> False
      Just res -> res == v
  exclude = difference
  varsInEnv = keys

instance HasVar EnvType where
  freeVars et = case et of
    NType  tag ty   -> freeVars ty
    GrType tag ty c -> freeVars ty ++ freeVars c
  vars  = freeVars

-- (+) : tenv1をtenv2に足す
-- tenv1とtenv2の両方に含まれるkeyは存在する場合は両者のリソースをマージする
(.++) :: TEnv -> TEnv -> TEnv
(.++) = unionWith concat
  where
    concat :: EnvType -> EnvType -> EnvType
    concat t1 t2 = if getType t1 /= getType t2
      then
        let message = "\nTwo env types are not equal." ++ 
                      "\n  envty1 :" ++ putDocString (ppP t1) ++
                      "\n  envty2 :" ++ putDocString (ppP t2)
        in error message
      else
        case (t1, t2) of
          (GrType tag1 t1 c1, GrType tag2 t2 c2) -> 
            if tag1 == tag2 && t1 == t2
              then GrType tag1 t1 (c1 .+ c2)
              else error "(.++) : Two EnvTy should have the same tag and type."
          _ -> error $ "Both types " ++ show t1 ++ " and " ++ show t2 ++ " should be types with resources."

-- (,) : tenv1をtenv2に足す
-- tenv1とtenv2の両方に含まれるkeyは存在する場合はerror
(.++.) :: TEnv -> TEnv -> TEnv
(.++.) = unionWithKey (\key t1 t2 -> error $ message key t1 t2)
  where
    message key t1 t2 = putDocString $ line <>
         ppP "Two environments have the same binding." <> line
      <> ppP "  key:" <+> ppP key <> line
      <> ppP "  t1: " <+> ppP t1 <> line
      <> ppP "  t2: " <+> ppP t2

-- tenv1をtenv2に足す
-- tenv1とtenv2の両方に含まれるkeyは存在する場合はtenv2で上書きする
-- PatやCaseから型環境が生成される際に用いる
(.+++) :: TEnv -> TEnv -> TEnv 
(.+++) = unionWithKey (\key t1 t2 -> t2)

(.**) :: Coeffect -> TEnv -> TEnv
(.**) c = Data.Map.map (mul c)
  where
    mul :: Coeffect -> EnvType -> EnvType
    mul c ty@(NType tag t) = error $ "All variables in the TEnv must have version resources, but ty:" ++ show ty ++ " does not have a resource."
    mul c1 (GrType tag t c2) = GrType tag t (c1 .* c2)

gradeTEnv :: TEnv -> TEnv
gradeTEnv = Data.Map.map gradeTy
  where
    gradeTy :: EnvType -> EnvType
    gradeTy (NType tag t)  = GrType tag t coeff1
    gradeTy ty@(GrType {}) = ty

-- genConstraintByと不等号が逆
genConstraint :: Coeffect -> TEnv -> Constraints
genConstraint c = Data.Map.foldl (\acc envty -> genCon c envty `landC` acc) CTop
  where
    genCon _ (NType _ _) = error "genConstraint: `genCon` cannot produce constraints from NType."
    genCon c1 (GrType _ _ c2) = CSubset c1 c2

-- genConstraintと不等号が逆
genConstraintBy :: Coeffect -> TEnv -> Constraints
genConstraintBy c = Data.Map.foldl (\acc envty -> genConBy c envty `landC` acc) CTop
  where
    genConBy c (NType _ _) = error "genConstraintBy: `genCon` cannot produce constraints from NType."
    genConBy c1 (GrType _ _ c2) = CSubset c2 c1

genConstraintPull :: Coeffect -> [Type] -> Constraints
genConstraintPull c []     = emptyConstraints 
genConstraintPull c ((TyBox c' _):ts) = CSubset c c' `landC` genConstraintPull c ts

data Env' = Env'
  { counter :: Int  -- 型変数の累積数
  , tEnv  :: TEnv   -- 型付け環境 / [x:A]
  , uEnv  :: UEnv   -- 単一化用型変数環境 / [X:κ]
  , rEnv  :: REnv   -- リソース環境 / -, r
  -- , cEnv  :: Constraints
  , log   :: Logs   -- ログ
  , logc  :: Int    -- ログのカウンタ(構文木の深さ)
  }
  deriving (Show)
type Env a = StateT Env' IO a

type Log = String
newtype Logs = Logs { logs :: [Log] }
  deriving (Eq, Show)

-- ^ Counter
setCounter :: Int -> Env ()
setCounter n = modify $ \env -> env { counter = n }

initCounter :: Int
initCounter = 0

initializeCounter :: Env ()
initializeCounter = setCounter initCounter

-- ^ TEnv
setTEnv :: TEnv -> Env ()
setTEnv tenv = modify $ \env -> env { tEnv = tenv }

initializeTEnv :: Env ()
initializeTEnv = setTEnv emptyEnv

putTEnv :: VarKey -> EnvType -> Env ()
putTEnv vk et = modify $ \env -> env { tEnv = insert vk et (tEnv env) }

-- ^ UEnv
setUEnv :: UEnv -> Env ()
setUEnv uenv = modify $ \env -> env { uEnv = uenv }

initializeUEnv :: Env ()
initializeUEnv = setUEnv emptyEnv

-- ^ REnv
emptyREnv :: REnv
emptyREnv = EmptyREnv

setREnv :: REnv -> Env ()
setREnv renv = modify $ \env -> env { rEnv = renv }

initializeREnv :: Env ()
initializeREnv = setREnv emptyREnv

mulREnv :: Coeffect -> REnv -> REnv
mulREnv c EmptyREnv  = REnv c
mulREnv c1 (REnv c2) = REnv (c1 .* c2)

-- ^ Constraints
-- getCEnv :: Env Constraints
-- getCEnv = state $ \env -> (cEnv env, env)

-- setCEnv :: Constraints -> Env ()
-- setCEnv c = modify $ \env -> env { cEnv = c }

-- initializeCEnv :: Env ()
-- initializeCEnv = modify $ \env -> env { cEnv = emptyConstraints }

-- putCEnv :: Constraints -> Env ()
-- putCEnv c = modify $ \env -> env { cEnv = c ++ cEnv env }

-- ^ Logs
setLogs :: Logs -> Env ()
setLogs logs = modify $ \env -> env { log = logs }

initializeLogs :: Env ()
initializeLogs = modify $ \env -> env { log = Logs [] }

putLog :: Log -> Env ()
putLog str = modify $ \env -> env { log = Logs (str : logs (log env)), logc = logc env - 1 }

reverseLogs :: Logs -> Logs
reverseLogs l = Logs $ reverse $ logs l

emptyLogs :: Logs
emptyLogs = Logs []

debug :: Doc ann ->Env ()
debug p = putLog $ putDocString p

debugE :: (PrettyAST a) => a ->Env ()
debugE p = putLog $ putDocString $ ppE p

debugP :: (PrettyAST a) => a ->Env ()
debugP p = putLog $ putDocString $ ppP p

-- ^ Log counter
initLC :: Int
initLC = -1

initializeLC :: Env ()
initializeLC = modify $ \env -> env { logc = initLC }

addLC :: Env ()
addLC = modify $ \env -> env { logc = 1 + logc env }

subLC :: Env ()
subLC = modify $ \env -> env { logc = logc env - 1 }

-- ^ initialize an environment without Counter (number of type variables)
initializeEnv :: Env ()
initializeEnv = do
  -- initializeCounter
  initializeTEnv
  initializeUEnv
  initializeREnv
  initializeLogs
  initializeLC

initEnv :: Env'
initEnv = Env' initCounter emptyEnv emptyEnv emptyREnv emptyLogs initLC

-- ^ Generate new type variable
--   counter++, return a generated type variable and new environment
genNewTyVar :: Kind -> Env Type
genNewTyVar kind = do
  c <- gets counter
  let name = prefixNewTyVar ++ show c
  uenv' <- gets (insertEnv name kind . uEnv)
  let tyvar = TyVar (Ident name)
  modify (\env -> env { uEnv = uenv', counter = c + 1 })
  return tyvar

---------------------------------------

basicTypes :: [String]
basicTypes = ["Int", "String", "Bool"]

intTy :: Type
intTy = TyCon (T.UnQual (Ident "Int"))

bxIntTy1 :: Type
bxIntTy1 =
  TyBox
    (TyVar (Ident "a0"))
    (TyCon (T.UnQual (Ident "Int")))

bxIntTy2 :: Type
bxIntTy2 =
  TyBox
    (TyVar (Ident "a1"))
    (TyCon (T.UnQual (Ident "Int")))

intToInt :: Type
intToInt = TyFun bxIntTy1 intTy

intToIntToInt :: Type
intToIntToInt = TyFun bxIntTy2 intToInt

------------------------------

instance PrettyAST EnvType where
  ppE (NType _ ty) = ppE ty
  ppE (GrType _ c ty) = brackets (ppE ty) <> ppE "_" <> parens (ppE c)
  ppP (NType _ ty) = ppP ty
  ppP (GrType _ ty c) = brackets (ppP ty) <> ppP "_" <> parens (ppP c)

instance PrettyAST TEnv where
  ppE = foldlWithKey
    (\acc k v -> acc <+>
      ppE k <> comma <> ppE v)
    emptyDoc
  ppP m
    | m == empty = emptyset
    | otherwise  = concatWith (surround $ comma <> space) $ Prelude.map (\(k,v) -> ppP k <> colon <> ppP v) (toList m)

instance PrettyAST UEnv where
  ppE = foldlWithKey
    (\acc k v -> acc <+>
      ppE k <> comma <> ppE v)
    emptyDoc
  ppP m
    | m == empty = emptyset
    | otherwise  = concatWith (surround $ comma <> space) $ Prelude.map (\(k,v) -> ppP k <> colon <> ppP v) (toList m)

instance PrettyAST REnv where
  ppE EmptyREnv = ppE "-"
  ppE (REnv c) = ppE c
  ppP EmptyREnv = ppP "-"
  ppP (REnv c) = ppP c

instance PrettyAST ExVarResources where
  ppE = foldlWithKey
    (\acc k v -> acc <+>
      ppE k <> comma <> ppE v)
    emptyDoc
  ppP m
    | m == empty = emptyset
    | otherwise  = concatWith (surround $ comma <> line) $ Prelude.map (\(k,v) -> ppP k <+> ppP "->" <+> ppP v) (toList m)

instance PrettyAST Logs where
  ppE (Logs l) = concatWith (surround line) $ Prelude.map ppE l
  ppP (Logs l) = concatWith (surround line) $ Prelude.map ppP l
