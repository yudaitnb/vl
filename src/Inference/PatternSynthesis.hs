{-# LANGUAGE FlexibleInstances #-}
module Inference.PatternSynthesis where

import Control.Monad.State

import Syntax.Env
import Syntax.Type as Type
import Syntax.Kind
import Syntax.LambdaVL
import Syntax.Substitution

import Inference.Kinding
-- import Inference.TypeUnification

import Util

type PatSynthRes = (TEnv, UEnv, Subst)

putPatSynthLog :: (PrettyAST l) => UEnv -> REnv -> Pat l -> PatSynthRes -> Env ()
putPatSynthLog oldu oldr pat patres = do
  let header = ppP "(PatSynt)"
      env = header <+> ppP oldu <> semicolon <+> ppP oldr <> vdash
      res = ppP pat <+> ppP "▷" <+> ppP patres
  putLog $ putDocString $ env <> res
  return ()

patternSynthesis :: (PrettyAST l) => Pat l -> Type -> Env PatSynthRes
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
          let result = (tenv', sigma, emptySubst)
          putPatSynthLog sigma renv p result
          return result
        -- pVar_gr 
        REnv r -> do
          let x = getName name
              tya' = GrType tya r
              tenv' = insertEnv x tya' emptyEnv
          hasTypeKind tya
          hasLabelsKind r
          let result = (tenv', sigma, emptySubst)
          putPatSynthLog sigma renv p result
          return result
    -- p□?
    PBox _ p      -> do
      sigma <- getUEnv
      renv  <- getREnv
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
          theta'' <- theta `comp` theta'
          let result = (delta, sigma'', theta'')
          putPatSynthLog sigma renv p result
          return result
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
          theta'' <- theta `comp` theta'
          let result = (delta, sigma'', theta'')
          putPatSynthLog sigma renv p result
          return result
    _ -> error "May the arguments be PLit or PWildCard?"
  
------------------------------

instance PrettyAST PatSynthRes where
  ppE (tenv, uenv, subst) = nest 2 $ parens $ ppE tenv <> semicolon <+> ppE uenv <> semicolon <+> ppE subst
  ppP (tenv, uenv, subst) = parens $ ppP tenv <> semicolon <+> ppP uenv <> semicolon <+> ppP subst
