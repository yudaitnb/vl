{-# LANGUAGE FlexibleInstances #-}
module Syntax.Substitution where

import Data.Maybe (fromMaybe)

import Syntax.Type
import Util

type Subst = [(String, Type)]

emptySubst :: Subst
emptySubst = []

singleSubst :: String -> Type -> Subst
singleSubst k v = [(k, v)]

findSubst :: String -> Subst -> Maybe Type
findSubst = lookup

(\\) :: Subst -> String -> Subst
(\\) s str = dropWhile (\(k,_) -> k == str) s

comp :: Subst -> Subst -> Subst
comp [] s2 = s2
comp ((alpha,tya):theta1) theta2 =
  case findSubst alpha theta2 of
    Nothing ->
      (alpha, tya):(theta1 `comp` theta2)
    Just tyb -> 
      let theta = unifySubst tya tyb in
      (alpha, typeSubstitution theta tya):(theta1 `comp` (theta2 \\ alpha))

-- ty1もty2もwell-definedなことを仮定
unifySubst :: Type -> Type -> Subst
unifySubst ty1 ty2 =
  case (ty1, ty2) of
    -- U_→
    (TyFun a b, TyFun a' b') ->
      let theta_1 = unifySubst a' a
          theta_2 = unifySubst (typeSubstitution theta_1 b) (typeSubstitution theta_1 b')
      in theta_1 `comp` theta_2
    -- U_box
    (TyBox r a, TyBox r' a') ->
      let theta_1 = unifySubst a a'
          theta_2 = unifySubst (typeSubstitution theta_1 r) (typeSubstitution theta_1 r') -- [TODO] typeSubstitution不要かも
      in theta_1 `comp` theta_2
    -- U_var=, U_var∃
    (TyVar alpha, ty2) ->
      if ty1 == ty2
        then emptySubst
        else singleSubst (getName alpha) ty2 
    (ty1, TyVar alpha) ->
      if ty1 == ty2
        then emptySubst 
        else singleSubst (getName alpha) ty1 
    -- U_=
    (ty1, ty2) ->
      if ty1 == ty2
        then emptySubst 
        else error ""

--------------------------

instance PrettyAST Subst where
  ppE = pretty
  ppP s = parens $ concatWith (surround $ comma <> space) $ map (\(k,v) -> pretty k <> pretty " ↦ " <> ppP v) s



typeSubstitution :: Subst -> Type -> Type
typeSubstitution s_table ty =
  let tysubst = typeSubstitution s_table in
  case ty of
    TyCon qName -> ty
    TyBottom    -> ty
    TyLabels _  -> ty
    TyVar name     -> fromMaybe ty (findSubst (getName name) s_table)
    TyFun ty1 ty2  ->
      let ty1' = tysubst ty1
          ty2' = tysubst ty2
      in TyFun ty1' ty2'
    TyBox coeff ty ->
      let coeff' = tysubst coeff
          ty'    = tysubst ty
      in TyBox coeff' ty'
    CAdd c1 c2 ->
      let c1' = tysubst c1
          c2' = tysubst c2
      in CAdd c1' c2'
    CMul c1 c2 ->
      let c1' = tysubst c1
          c2' = tysubst c2
      in CMul c1' c2'