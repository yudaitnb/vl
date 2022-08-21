module Kinding (
  kind
  , isTypeKind
  , isLabelsKind
) where

import Control.Monad.State

import Syntax.Type
import Syntax.Kind
import Syntax.Env


isTypeKind :: Monad m => UEnv -> Type -> m ()
isTypeKind uenv ty = if TypeKind == kindTy uenv ty
  then return ()
  else error ("The type of ty:" ++ show ty ++ " does not have TypeKind.")

isLabelsKind :: Monad m => UEnv -> Type -> m ()
isLabelsKind uenv ty = if LabelsKind == kindTy uenv ty
  then return ()
  else error ("The type of ty:" ++ show ty ++ " does not have TypeKind.")

kindTy :: UEnv -> Type -> Kind
kindTy uenv ty = evalState (kind ty) (KindEnv' uenv)

newtype KindEnv' = KindEnv' { uEnv :: UEnv }
type KindEnv a = State KindEnv' a

basicType :: [String]
basicType = ["Int", "String", "Char"]

getUEnv :: KindEnv UEnv
getUEnv = state $ \(KindEnv' s) -> (s, KindEnv' s)

setUEnv :: UEnv -> KindEnv ()
setUEnv uenv = state $ \(KindEnv' _) -> ((), KindEnv' uenv)

kind :: Type -> KindEnv Kind
kind ty = case ty of
  -- κ_ty
  TyCon qName -> if getName qName `elem` basicType
                  then return TypeKind
                  else error ""
  -- κ_→
  TyFun t1 t2 -> do
    k1 <- kind t1
    k2 <- kind t2
    if k1 == TypeKind && k2 == TypeKind
      then return TypeKind
      else error $ "Both t1:" ++ show t1 ++ " and t2:" ++ show t2 ++ " of TyFun t1 t2 must have TypeKind."
  -- κ_var
  TyVar name  -> do
    sigma <- getUEnv
    let kappa = sigma ! getName name
    return kappa
  -- κ_□
  TyBox c ty  -> do
    r   <- kind c
    tya <- kind ty
    if (r == LabelsKind) && (tya == TypeKind)
      then return TypeKind
      else error $ "c:" ++ show c ++ " must have LabelsKind and ty:" ++ show ty ++ " must have TypeKind."
  -- κ_0
  TyBottom    -> return LabelsKind
  -- κ_1, κ_label
  TyLabels s  -> return LabelsKind -- [TODO] sをCollected Labelsか検査する
