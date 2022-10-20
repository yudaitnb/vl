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
  lc <- gets logc
  let header = ppP "(Kinding)"
      env = header 
        -- <+> ppP oldu -- [UEnv]
        <> vdash
      depth = concatWith (<>) $ replicate lc (ppP "|")
      res = ppP ty <+> colon <+> ppP kind
  putLog $ putDocString $
    depth <>
    env <>
    res
  return ()

hasTypeKind :: Type -> Env ()
hasTypeKind ty = do
  k <- kind ty
  uenv <- gets uEnv
  if TypeKind == k
    then return ()
    else error $ putDocString $ line <>
      ppP "The type does not have TypeKind." <> line <>
      ppP "    ty:" <+> ppP ty <> line <>
      ppP "  uenv:" <+> ppP uenv

hasLabelsKind :: Type -> Env ()
hasLabelsKind ty = do
  k <- kind ty
  uenv <- gets uEnv
  if LabelsKind == k
    then return ()
    else error $ putDocString $ line <>
      ppP "The type does not have LabelsKind." <> line <>
      ppP "    ty:" <+> ppP ty <> line <>
      ppP "  uenv:" <+> ppP uenv

kind :: Type -> Env Kind
kind ty = do
  addLC
  case ty of
    -- κ_ty
    TyCon qName -> do
      sigma <- gets uEnv
      if getName qName `elem` basicType
        then do
          let result = TypeKind
          putKindingLog sigma ty result
          return result
        else error ""
    -- κ_→
    TyFun t1 t2 -> do
      sigma <- gets uEnv
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
      sigma <- gets uEnv
      case lookupEnv (getName name) sigma of
        Nothing -> do
          uenv <- gets uEnv
          error $ "`kind` expects a type variable to be in uenv, but it isn't.\n"
            ++ "  name: " ++ show name ++ "\n"
            ++ "  uenv: " ++ show uenv
        Just kappa -> do
          putKindingLog sigma ty kappa
          return kappa
    -- κ_□
    TyBox r ty  -> do
      sigma <- gets uEnv
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
      sigma <- gets uEnv
      let result = LabelsKind
      putKindingLog sigma ty result
      return result
    -- κ_1, κ_label
    TyLabels s  -> do
      sigma <- gets uEnv
      let result = LabelsKind
      putKindingLog sigma ty result
      return result -- [TODO] sをCollected Labelsか検査する
    
    CAdd c1 c2  -> do
      sigma <- gets uEnv
      hasLabelsKind c1
      hasLabelsKind c2
      let result = LabelsKind
      putKindingLog sigma ty result
      return result

    CMul c1 c2  -> do
      sigma <- gets uEnv
      hasLabelsKind c1
      hasLabelsKind c2
      let result = LabelsKind
      putKindingLog sigma ty result
      return result
    
    -- CSubset c1 c2 -> do
    --   sigma <- gets uEnv
    --   hasLabelsKind c1
    --   hasLabelsKind c2
    --   let result = ConstraintKind
    --   putKindingLog sigma ty result
    --   return result
