module PatternSynthesis (patSynth) where

import Control.Monad.State

import Syntax.Env
import Syntax.Type as Type
import Syntax.Kind
import Syntax.LambdaVL
import Syntax.Substitution

import Kinding
import TypeUnification

patSynth :: Int -> UEnv -> REnv -> Pat l -> Type -> (TEnv, SubstMap)
patSynth c uenv renv pat ty = evalState (patternSynthesis pat ty) (PatternEnv' c uenv renv)

data PatternEnv' = PatternEnv'
  { counter :: Int
  , uEnv :: UEnv   -- 
  , rEnv :: REnv   -- リソース環境
  }
type Env a = State PatternEnv' a

primEnv :: PatternEnv'
primEnv = PatternEnv' 0 emptyUEnv emptyREnv

prefixNewTyVar :: String
prefixNewTyVar = "_tyvar_"

getUEnv :: Env UEnv
getUEnv = state $ \env@(PatternEnv' _ u _) -> (u, env)

getREnv :: Env REnv
getREnv = state $ \env@(PatternEnv' _ _ r) -> (r, env)

setUEnv :: UEnv -> Env ()
setUEnv uenv = state $ \(PatternEnv' c _ r) -> ((), PatternEnv' c uenv r)

setREnv :: REnv -> Env ()
setREnv renv = state $ \(PatternEnv' c u _) -> ((), PatternEnv' c u renv)

genNewTyVar :: Kind -> Env Type
genNewTyVar kind = state $ \(PatternEnv' c u r) ->
  let strvar' = prefixNewTyVar ++ show c
      counter' = c + 1
      uenv' = addUEnv strvar' kind u
      tyvar = TyVar (Type.Ident strvar')
  in (tyvar, PatternEnv' counter' uenv' r)

patternSynthesis :: Pat l -> Type -> Env (TEnv, SubstMap)
patternSynthesis p tya = case p of
    -- PLit _ sign lit -> 
    -- PWildCard _     ->  

    -- PVar_lin, pVar_gr 
    PVar _ name     -> do
      uenv <- getUEnv
      renv <- getREnv
      case renv of
        EmptyREnv -> do
          let x = Syntax.LambdaVL.getName name
              tya' = NType tya
              tenv' = addTEnv x tya' emptyTEnv
          isTypeKind uenv tya
          return (tenv', emptySubst)
        REnv r -> do
          let x = Syntax.LambdaVL.getName name
              tya' = GrType tya r
              tenv' = addTEnv x tya' emptyTEnv
          isTypeKind uenv tya
          isLabelsKind uenv r
          return (tenv', emptySubst)
    PBox _ p      -> do
      uenv <- getUEnv
      renv <- getREnv
      case renv of
        EmptyREnv -> do
          alpha <- genNewTyVar LabelsKind
          beta  <- genNewTyVar TypeKind
          sigma' <- getUEnv
          isLabelsKind sigma' alpha
          setREnv $ REnv alpha
          (delta, theta) <- patternSynthesis p beta
          sigma'' <- getUEnv
          let theta' = unify sigma' tya (TyBox alpha beta)
              theta'' = comp theta theta'
          return (delta, theta'')
        renv@(REnv _) -> do
          alpha <- genNewTyVar LabelsKind
          beta  <- genNewTyVar TypeKind
          sigma' <- getUEnv
          isLabelsKind sigma' alpha
          setREnv $ mulREnv alpha renv
          (delta, theta) <- patternSynthesis p beta
          sigma'' <- getUEnv
          let theta' = unify sigma' tya (TyBox alpha beta)
              theta'' = comp theta theta'
          return (delta, theta'')
    _ -> error "May the arguments be PLit or PWildCard?"