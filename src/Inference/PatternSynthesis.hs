module Inference.PatternSynthesis where

import Control.Monad.State

import Language.LambdaVL

import Syntax.Env
import Syntax.Type as Type
import Syntax.Kind

import Syntax.Substitution

import Inference.Kinding
-- import Inference.TypeUnification

import Util
import Syntax.Common

type PatSynthRes = (TEnv, UEnv, Subst)

putPatSynthLog :: UEnv -> REnv -> Pat SrcSpanInfo -> PatSynthRes -> Env ()
putPatSynthLog oldu oldr pat patres = do
  lc <- gets logc
  let header = ppP "(PatSynt)"
      env = header <+>
        -- ppP oldu <> semicolon <+>  -- [UEnv]
        ppP oldr <> vdash
      depth = concatWith (<>) $ replicate lc (ppP "|")
      res = ppP pat <+> ppP "▷" <+> ppP patres
  putLog $ putDocString $
    depth <>
    env <>
    res
  return ()

patternSynthesis :: Pat SrcSpanInfo -> Type -> Env PatSynthRes
patternSynthesis p tya = do
  addLC
  case p of
    -- pLit, [pLit]
    PLit _ sign lit -> do
      renv <- gets rEnv
      sigma <- gets uEnv
      -- rがある場合はwell-definedness (1 <= r) を検査すべき
      -- case renv of
      --   EmptyREnv -> do
      --   REnv r -> do
      let resty = case lit of
            Char {}   -> tychar
            String {} -> tystring
            Int {}    -> tyint
      theta <- typeUnification resty tya
      let result = (emptyEnv, sigma, theta)
      putPatSynthLog sigma renv p result
      return result 

    -- p_
    PWildCard _     -> do 
      renv <- gets rEnv
      sigma <- gets uEnv
      -- rがある場合はwell-definedness (0 <= r) を検査すべき
      -- case renv of
      --   EmptyREnv -> do
      --   REnv r -> do
      hasTypeKind tya
      let result = (emptyEnv, sigma, emptySubst)
      putPatSynthLog sigma renv p result
      return result 

    -- pVar_?
    PVar _ name     -> do
      renv <- gets rEnv
      sigma <- gets uEnv
      case renv of
        -- PVar_lin
        EmptyREnv -> do
          let vk = UQVar $ getName name
              tya' = NType Local tya
              tenv' = insertEnv vk tya' emptyEnv
          hasTypeKind tya
          let result = (tenv', sigma, emptySubst)
          putPatSynthLog sigma renv p result
          return result
        -- pVar_gr 
        REnv r -> do
          let vk = UQVar $ getName name
              tya' = GrType Local tya r
              tenv' = insertEnv vk tya' emptyEnv
          hasTypeKind tya
          hasLabelsKind r
          let result = (tenv', sigma, emptySubst)
          putPatSynthLog sigma renv p result
          return result
    -- p□, p□ctx
    PBox _ p'      -> do
      sigma <- gets uEnv
      renv  <- gets rEnv
      alpha <- genNewTyVar LabelsKind
      beta  <- genNewTyVar TypeKind
      sigma' <- gets uEnv
      hasLabelsKind alpha -- [TODO] tribial
      let newREnv = case renv of
            EmptyREnv -> REnv alpha
            REnv _    -> mulREnv alpha renv
      setREnv newREnv
      (delta, sigma'', theta) <- patternSynthesis p' beta
      theta' <- typeUnification (TyBox alpha beta) tya
      theta'' <- theta `comp` theta'
      let result = (delta, sigma'', theta'')
      putPatSynthLog sigma renv p result
      return result

    -- p(), [p()]
    PTuple _ ps      -> do
      sigma <- gets uEnv
      renv  <- gets rEnv
      alphas <- forM (replicate (length ps) 0) $ \_ -> genNewTyVar TypeKind
      theta' <- typeUnification (TyTuple alphas) tya
      patSynthRess <- forM (zip ps alphas) $ \(pi, alphai) -> do
        -- sigma' <- gets uEnv
        hasTypeKind alphai -- [TODO] tribial, 前に出してもよい？
        setREnv renv
        patternSynthesis pi alphai
      sigma' <- gets uEnv
      let sigmaLast = if null patSynthRess
                        then sigma'
                        else case last patSynthRess of (_, sigmaLast, _) -> sigmaLast
      let deltaSum = foldr (\(gammai, _, _) accGamma -> gammai .++. accGamma) emptyEnv patSynthRess
      thetaSum <- foldM comp emptySubst $ map (\(_, _, thetai) -> thetai) patSynthRess
      thetaRes <- theta' `comp` thetaSum
      let result = (deltaSum, sigmaLast, thetaRes)
      putPatSynthLog sigma renv p result
      return result

    -- p[], [p[]]
    PList _ ps      -> do
      sigma <- gets uEnv
      renv  <- gets rEnv
      alpha <- genNewTyVar TypeKind
      theta' <- typeUnification (TyList alpha) tya
      patSynthRess <- forM (zip ps (replicate (length ps) alpha)) $ \(pi, alphai) -> do
        -- sigma' <- gets uEnv
        hasTypeKind alphai -- [TODO] tribial, 前に出してもよい？
        setREnv renv
        patternSynthesis pi alphai
      sigma' <- gets uEnv
      let sigmaLast = if null patSynthRess
                        then sigma'
                        else case last patSynthRess of (_, sigmaLast, _) -> sigmaLast
      let deltaSum = foldr (\(gammai, _, _) accGamma -> gammai .++. accGamma) emptyEnv patSynthRess
      thetaSum <- foldM comp emptySubst $ map (\(_, _, thetai) -> thetai) patSynthRess
      thetaRes <- theta' `comp` thetaSum
      let result = (deltaSum, sigmaLast, thetaRes)
      putPatSynthLog sigma renv p result
      return result

    -- pcon, [pcon]
    PApp _ (Special _ spcon) ps -> do
      sigma <- gets uEnv
      renv  <- gets rEnv
      error "PApp !!"
      -- case spcon of
      --   UnitCon _ -> do
      --   ListCon _ -> do
      --   TupleCoCon _ _ _ do
      --   Cons _ -> do
      --     alphas <- forM (replicate (length ps) 0) $ \_ -> genNewTyVar TypeKind
      --     theta' <- typeUnification (TyTuple alphas) tya
      --     patSynthRess <- forM (zip ps alphas) $ \(pi, alphai) -> do
      --       -- sigma' <- gets uEnv
      --       hasTypeKind alphai -- [TODO] tribial, 前に出してもよい？
      --       setREnv renv
      --       patternSynthesis pi alphai
      --     sigma' <- gets uEnv
      --     let sigmaLast = if null patSynthRess
      --                       then sigma'
      --                       else case last patSynthRess of (_, sigmaLast, _) -> sigmaLast
      --     let deltaSum = foldr (\(gammai, _, _) accGamma -> gammai .++. accGamma) emptyEnv patSynthRess
      --     thetaSum <- foldM comp emptySubst $ map (\(_, _, thetai) -> thetai) patSynthRess
      --     thetaRes <- theta' `comp` thetaSum
      --     let result = (deltaSum, sigmaLast, thetaRes)
      --     putPatSynthLog sigma renv p result
      --     return result

    PInfixApp _ p1 qn p2 -> do
      sigma <- gets uEnv
      renv  <- gets rEnv
      case qn of
        Special _ spcon -> do
          case spcon of
            -- UnitCon _ -> do
            -- ListCon _ -> do
            -- TupleCoCon _ _ _ do
            Cons _ -> do
              -- p1 : alpha
              -- p2 : List alpha
              alpha <- genNewTyVar TypeKind
              (gamma1, _, theta1) <- patternSynthesis p1 alpha
              (gamma2, sigma2, theta2) <- patternSynthesis p2 (TyList alpha)
              theta3 <- theta1 `comp` theta2
              let result = (gamma1 .++. gamma2, sigma2, theta3)
              putPatSynthLog sigma renv p result
              return result

            _ -> error "Pattern inference for PInfixApp is not defined except for Cons."
        _ -> error "Pattern inference for PINfixApp is not defined except for SpecialCon."
      
    pat -> error $ "patternSynthesis does not support the given pattern:\n" ++ show pat
  
------------------------------

instance PrettyAST PatSynthRes where
  ppE (tenv, uenv, subst) = nest 2 $ parens $ ppE tenv <> semicolon <+> ppE uenv <> semicolon <+> ppE subst
  ppP (tenv, uenv, subst) = parens $
    ppP tenv <> semicolon
    -- <+> ppP uenv <> semicolon [UEnv]
    <+> ppP subst
