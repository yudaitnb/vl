module PatternSynthesis where

import Control.Monad.State

import Syntax.Env
import Syntax.Type as Type
import Syntax.Kind
import Syntax.LambdaVL
import Syntax.Substitution

import Kinding
import TypeUnification

patternSynthesis :: Pat l -> Type -> Env (TEnv, UEnv, SubstMap)
patternSynthesis p tya = case p of
    -- PLit _ sign lit -> 
    -- PWildCard _     ->  

    -- pVar_?
    PVar _ name     -> do
      renv <- getREnv
      sigma <- getUEnv
      case renv of
        -- PVar_lin
        EmptyREnv -> do
          let x = getName name
              tya' = NType tya
              tenv' = insertEnv x tya' emptyEnv
          hasTypeKind tya
          return (tenv', sigma, emptySubst)
        -- pVar_gr 
        REnv r -> do
          let x = getName name
              tya' = GrType tya r
              tenv' = insertEnv x tya' emptyEnv
          hasTypeKind tya
          hasLabelsKind r
          return (tenv', sigma, emptySubst)
    -- p□?
    PBox _ p      -> do
      renv <- getREnv
      case renv of
        -- p□Noctx
        EmptyREnv -> do
          alpha <- genNewTyVar LabelsKind
          beta  <- genNewTyVar TypeKind
          sigma' <- getUEnv
          hasLabelsKind alpha -- [TODO] tribial
          setREnv $ REnv alpha
          (delta, sigma'', theta) <- patternSynthesis p beta
          theta' <- typeUnification tya (TyBox alpha beta)
          return (delta, sigma'', comp theta theta')
        -- p□ctx
        renv@(REnv _) -> do
          alpha <- genNewTyVar LabelsKind
          beta  <- genNewTyVar TypeKind
          sigma' <- getUEnv
          hasLabelsKind alpha -- [TODO] tribial
          setREnv $ mulREnv alpha renv
          (delta, sigma'', theta) <- patternSynthesis p beta
          sigma'' <- getUEnv
          theta' <- typeUnification tya (TyBox alpha beta)
          return (delta, sigma'', comp theta theta')
    _ -> error "May the arguments be PLit or PWildCard?"