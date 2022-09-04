module Inference.Kinding (
  kind
  , hasTypeKind
  , hasLabelsKind
) where

import Control.Monad.State

import Syntax.Type
import Syntax.Kind
import Syntax.Env

import Util

putKindingLog :: UEnv -> Type -> Kind -> Env ()
putKindingLog oldu ty kind = do
  let header = ppP "(Kinding)"
      env = header <+> ppP oldu <> vdash
      res = ppP ty <+> colon <+> ppP kind
  putLog $ putDocString $ env <> res
  return ()

hasTypeKind :: Type -> Env ()
hasTypeKind ty = do
  k <- kind ty
  if TypeKind == k
    then return ()
    else error ("The type of ty:" ++ show ty ++ " does not have TypeKind.")

hasLabelsKind :: Type -> Env ()
hasLabelsKind ty = do
  k <- kind ty
  if LabelsKind == k
    then return ()
    else error ("The type of ty:" ++ show ty ++ " does not have TypeKind.")

kind :: Type -> Env Kind
kind ty = case ty of
  -- κ_ty
  TyCon qName -> do
    sigma <- getUEnv
    if getName qName `elem` basicType
      then do
        let result = TypeKind
        putKindingLog sigma ty result
        return result
      else error ""
  -- κ_→
  TyFun t1 t2 -> do
    sigma <- getUEnv
    k1 <- kind t1
    k2 <- kind t2
    if k1 == TypeKind && k2 == TypeKind
      then do
        let result = TypeKind
        putKindingLog sigma ty result
        return result
      else error $ "Both kinds of t1 and t2 in `TyFun t1 t2` must have TypeKind, but it isn't.\n"
        ++ "  t1: " ++ show k1 ++ "\n"
        ++ "  t2: " ++ show k2
  -- κ_var
  TyVar name  -> do
    sigma <- getUEnv
    case lookupEnv (getName name) sigma of
      Nothing -> do
        uenv <- getUEnv
        error $ "`kind` expects a type variable to be in uenv, but it isn't.\n"
          ++ "  name:" ++ show name ++ "\n"
          ++ "  uenv:" ++ show uenv
      Just kappa -> do
        putKindingLog sigma ty kappa
        return kappa
  -- κ_□
  TyBox r ty  -> do
    sigma <- getUEnv
    k1 <- kind r
    k2 <- kind ty
    if (k1 == LabelsKind) && (k2 == TypeKind)
      then do
        let result = TypeKind
        putKindingLog sigma ty result
        return result
      else error $ "r and ty in `TyBox r ty` must have Labelskinds and TypeKind respectively, but they don't.\n"
        ++ "   kind of r: " ++ show k1 ++ "\n"
        ++ "  kind of ty: " ++ show k2
  -- κ_0
  TyBottom    -> do
    sigma <- getUEnv
    let result = LabelsKind
    putKindingLog sigma ty result
    return result
  -- κ_1, κ_label
  TyLabels s  -> do
    sigma <- getUEnv
    let result = LabelsKind
    putKindingLog sigma ty result
    return result -- [TODO] sをCollected Labelsか検査する
  
  CAdd c1 c2  -> do
    sigma <- getUEnv
    hasLabelsKind c1
    hasLabelsKind c2
    let result = LabelsKind
    putKindingLog sigma ty result
    return result

  CMul c1 c2  -> do
    sigma <- getUEnv
    hasLabelsKind c1
    hasLabelsKind c2
    let result = LabelsKind
    putKindingLog sigma ty result
    return result