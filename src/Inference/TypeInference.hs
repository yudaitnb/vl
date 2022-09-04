{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Inference.TypeInference  where

import qualified Syntax.LambdaVL as VL
import Data.Maybe (fromMaybe)
import Control.Monad.State
import Data.List (foldl, map)
import Control.Arrow (second)
import Prelude hiding (lookup)

import Syntax.Type
import Syntax.Kind
import Syntax.Env
import Syntax.Substitution

-- import Inference.TypeUnification
import Inference.Kinding
import Inference.PatternSynthesis

import Util

type TyInfRes = (Type, TEnv, UEnv, Subst, Constraints)

typeOf :: Typable ast => Env' -> ast -> TyInfRes
typeOf env ast = evalState (infer ast) env

typeOfWithLogs :: Typable ast => Env' -> ast -> (TyInfRes, Logs)
typeOfWithLogs env ast =
  let (res, Env' _ _ _ _ l) = runState (infer ast) env
  in (res, l)

putTyInfLog :: (PrettyAST l) => UEnv -> TEnv -> VL.Exp l -> TyInfRes -> Env ()
putTyInfLog oldu oldt exp tyInfRes = do
  let header = ppP "(TypeInf)"
      env = header <+> ppP oldu <> semicolon <+> ppP oldt <> vdash
      res = ppP exp <+> ppP "=>" <+> ppP tyInfRes
  putLog $ putDocString $ env <> res
  return ()

class Typable ast where
  infer :: ast -> Env TyInfRes

instance (PrettyAST l) => Typable (VL.Exp l) where
  infer exp = case exp of
    -- ^ ⇒_int
    VL.Lit _ (VL.Char {}) -> do
      gamma <- getTEnv
      sigma <- getUEnv
      let tychar = TyCon (UnQual (Ident "Char"))
          result = (tychar, emptyEnv, sigma, emptySubst, emptyConstraints)
      putTyInfLog sigma gamma exp result
      return result
    VL.Lit _ (VL.String {}) -> do
      gamma <- getTEnv
      sigma <- getUEnv
      let tystr = TyCon (UnQual (Ident "String"))
          result = (tystr, emptyEnv, sigma, emptySubst, emptyConstraints)
      putTyInfLog sigma gamma exp result
      return result
    VL.Lit _ (VL.Int {}) -> do
      gamma <- getTEnv
      sigma <- getUEnv
      let tyint = TyCon (UnQual (Ident "Int"))
          result = (tyint, emptyEnv, sigma, emptySubst, emptyConstraints)
      putTyInfLog sigma gamma exp result
      return result

    -- ^ ⇒_Var?
    VL.Var _ qName -> do
      gamma <- getTEnv
      sigma <- getUEnv
      let x = getName qName
      case lookupEnv x gamma of
        Nothing -> do
          logs <- getLogs
          let message = "⇒_Lin/Gr expects a variable: "
                        ++ show x 
                        ++ " to be in tenv, but it was not found.\n"
                        ++ putDocString (ppP logs)
          error message
        -- ^ ⇒_Lin
        Just ty@(NType tya) -> do
          let result = (tya, makeEnv [(x, ty)], sigma, emptySubst, emptyConstraints)
          putTyInfLog sigma gamma exp result
          return result
        -- ^ ⇒_Gr
        Just ty@(GrType tya r) -> do
          hasLabelsKind r
          let result = (tya, makeEnv [(x, GrType tya coeff1)], sigma, emptySubst, emptyConstraints)
          putTyInfLog sigma gamma exp result
          return result

    -- ^ ⇒_app
    VL.App _ e1 e2 -> do
      gamma <- getTEnv
      sigma1 <- getUEnv
      (tya, delta1, sigma2, theta_1, c1) <- infer e1
      setUEnv sigma2
      (tya', delta2, sigma3, theta_2, c2) <- infer e2
      beta <- genNewTyVar TypeKind
      let name = getName beta
          sigma4 = insertEnv name TypeKind sigma3
          tyfun = TyFun tya' beta
      setUEnv sigma4
      theta_3 <- typeUnification tya tyfun
      theta_4 <- theta_1 `comp` theta_2 >>= comp theta_3
      let beta' = typeSubstitution theta_4 beta
          result = (beta', delta1 .++ delta2, sigma3, theta_4, c1 `landC` c2)
      putTyInfLog sigma1 gamma exp result
      return result

    -- ^ ⇒_pr
    VL.Pr _ t -> do
      gamma <- getTEnv
      sigma1 <- getUEnv
      let gamma' = gradeTEnv (filterEnvBy (VL.freeVars t) gamma)
      setTEnv gamma'
      (tya, delta, sigma2, theta, c1) <- infer t
      alpha <- genNewTyVar LabelsKind
      let c2 = genConstraint alpha gamma'
          name = getName alpha
          result =  ( TyBox alpha tya
                    , alpha .** delta
                    , insertEnv name LabelsKind sigma2
                    , theta,
                    c1 `landC` c2)
      putTyInfLog sigma1 gamma exp result
      return result

    -- ^ ⇒_abs
    VL.Lambda _ p t -> do
      gamma <- getTEnv
      sigma <- getUEnv
      alpha <- genNewTyVar LabelsKind
      let name = getName alpha
      sigma1' <- getUEnv
      setREnv emptyREnv
      (gamma', sigma2, theta) <- patternSynthesis p alpha
      setUEnv sigma2
      setTEnv $ gamma .++ gamma' -- [TODO] , が .++  で問題ないか?
      (tyb, delta, sigma3, theta', c1) <- infer t
      theta'' <- theta `comp` theta'
      let result =  ( -- typeSubstitution theta (TyFun alpha tyb)
                      typeSubstitution theta'' (TyFun alpha tyb) -- ?
                    , delta `exclude` gamma'
                    , insertEnv name LabelsKind sigma3
                    , theta''
                    , c1)
      putTyInfLog sigma gamma exp result
      return result

    _ -> error "The function `infer` is not defined for a given expression."

newtype TypedExp = TypedExp (String, Type, Constraints)
  deriving (Eq, Ord, Show)

type TypedExpWithLog = (TypedExp, Logs)

getInterface :: (PrettyAST l) => VL.Module l -> [TypedExpWithLog]
getInterface (VL.Module _ _ _ decls) =
  let decls' = map (second reverseLogs . typeDecl) decls -- Reverse logs
  in decls'
  where
    typeDecl :: (PrettyAST l) => VL.Decl l -> TypedExpWithLog
    typeDecl (VL.PatBind _ pat exp) =
      let name = getName pat
          env = Env' 0 basicTEnv emptyEnv emptyREnv emptyLogs
          ((ty, _, _, _, c), l) = typeOfWithLogs env exp
      in (TypedExp (name, ty, c), l)


------------------------------

instance PrettyAST TyInfRes where
  ppE (ty, tenv, uenv, subst, c) = let sep = ppE "; " in
    ppE ty <> sep <> ppE tenv <> sep <> ppE uenv <> sep <> ppE subst <> sep <> ppE c
  ppP (ty, tenv, uenv, subst, c) = parens $ ppP ty <> semicolon <+> ppP tenv <> semicolon <+> ppP uenv <> semicolon <+> ppP subst <> semicolon <+> ppP c

instance PrettyAST TypedExp where
  ppE (TypedExp (str, ty, c)) = ppE str <+> colon <+> ppE ty <+> ppE "|" <+> ppE c -- [TODO]
  ppP (TypedExp (str, ty, c)) = ppP str <+> colon <+> ppP ty <+> ppP "|" <+> ppP c