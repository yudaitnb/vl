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

import Inference.TypeUnification
import Inference.Kinding
import Inference.PatternSynthesis

import Util

type TyInfRes = (Type, TEnv, UEnv, Subst)

typeOf :: Typable ast => Env' -> ast -> TyInfRes
typeOf env ast = evalState (infer ast) env

typeOfWithLogs :: Typable ast => Env' -> ast -> (TyInfRes, Logs)
typeOfWithLogs env ast =
  let (res, Env' _ _ _ _ l) = runState (infer ast) env
  in (res, l)

putTyInfLog :: (PrettyAST l) => UEnv -> TEnv -> VL.Exp l -> TyInfRes -> Env ()
putTyInfLog oldu oldt exp tyInfRes = do
  let header = pretty "(TypeInf)"
      env = header <+> ppP oldu <> semicolon <+> ppP oldt <> vdash
      res = ppP exp <+> pretty "=>" <+> ppP tyInfRes
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
          result = (tychar, emptyEnv, sigma, emptySubst)
      putTyInfLog sigma gamma exp result
      return result
    VL.Lit _ (VL.String {}) -> do
      gamma <- getTEnv
      sigma <- getUEnv
      let tystr = TyCon (UnQual (Ident "String"))
          result = (tystr, emptyEnv, sigma, emptySubst)
      putTyInfLog sigma gamma exp result
      return result
    VL.Lit _ (VL.Int {}) -> do
      gamma <- getTEnv
      sigma <- getUEnv
      let tyint = TyCon (UnQual (Ident "Int"))
          result = (tyint, emptyEnv, sigma, emptySubst)
      putTyInfLog sigma gamma exp result
      return result

    -- ^ ⇒_Var?
    VL.Var _ qName -> do
      gamma <- getTEnv
      sigma <- getUEnv
      let x = getName qName
      case lookupEnv x gamma of
        Nothing ->
          let message = "⇒_Lin/Gr expects a variable: " ++ show x ++ " to be in tenv, but it was not found."
          in error message
        -- ^ ⇒_Lin
        Just ty@(NType tya) -> do
          let result = (tya, makeEnv [(x, ty)], sigma, emptySubst)
          putTyInfLog sigma gamma exp result
          return result
        -- ^ ⇒_Gr
        Just ty@(GrType tya r) -> do
          hasLabelsKind r
          let result = (tya, makeEnv [(x, GrType tya coeff1)], sigma, emptySubst)
          putTyInfLog sigma gamma exp result
          return result

    -- ^ ⇒_app
    VL.App _ e1 e2 -> do
      gamma <- getTEnv
      sigma1 <- getUEnv
      (tya, delta1, sigma2, theta_1) <- infer e1
      setUEnv sigma2
      (tya', delta2, sigma3, theta_2) <- infer e2
      beta <- genNewTyVar TypeKind
      let name = getName beta
          sigma4 = insertEnv name TypeKind sigma3
          tyfun = TyFun tya' beta
      setUEnv sigma4
      theta_3 <- typeUnification tya tyfun
      let theta_4 = (theta_1 `comp` theta_2) `comp` theta_3
          beta' = typeSubstitution theta_4 beta
          result = (beta', delta1 .++ delta2, sigma3, theta_4)
      putTyInfLog sigma1 gamma exp result
      return result

    -- ^ ⇒_pr
    VL.Pr _ t -> do
      gamma <- getTEnv
      sigma1 <- getUEnv
      let gamma' = gradeTEnv (filterEnvBy (VL.freeVars t) gamma)
      setTEnv gamma'
      (tya, delta, sigma2, theta) <- infer t
      alpha <- genNewTyVar LabelsKind
      let name = getName alpha
          result =  ( TyBox alpha tya
                    , alpha .** delta
                    , insertEnv name LabelsKind sigma2
                    , theta)
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
      (tyb, delta, sigma3, theta') <- infer t
      let result =  ( typeSubstitution theta (TyFun alpha tyb)
                      -- typeSubstitution (theta `comp` theta') (TyFun alpha tyb) -- ?
                    , delta `exclude` gamma'
                    , insertEnv name LabelsKind sigma3
                    , theta `comp` theta')
      putTyInfLog sigma gamma exp result
      return result

    _ -> error "The function `infer` is not defined for a given expression."

newtype TypedExp = TypedExp (String, Type)
  deriving (Eq, Ord, Show)

type TypedExpWithLog = (TypedExp, Logs)

getInterface :: (PrettyAST l) => VL.Module l -> [TypedExpWithLog]
getInterface (VL.Module _ _ _ decls) = map (second reverse . typeDecl) decls -- Reverse logs
  where
    typeDecl :: (PrettyAST l) => VL.Decl l -> TypedExpWithLog
    typeDecl (VL.PatBind _ pat exp) =
      let name = getName pat
          env = Env' 0 basicTEnv emptyEnv emptyREnv []
          ((ty, _, _, _), l) = typeOfWithLogs env exp
      in (TypedExp (name, ty), l)


------------------------------

instance Pretty TyInfRes where
  pretty (ty, tenv, uenv, subst) =
    let sep = pretty "; " in
    pretty ty <> sep <> pretty tenv <> sep <> pretty uenv <> sep <> pretty subst

instance Pretty TypedExp where
  pretty (TypedExp (str,ty)) = pretty str <+> colon <+> ppP ty <> line

------------------------------

instance PrettyAST TyInfRes where
  ppE (ty, tenv, uenv, subst) = nest 2 $ parens $ ppE ty <> semicolon <+> ppE tenv <> semicolon <+> ppE uenv <> semicolon <+> ppE subst
  ppP (ty, tenv, uenv, subst) = parens $ ppP ty <> semicolon <+> ppP tenv <> semicolon <+> ppP uenv <> semicolon <+> ppP subst

instance PrettyAST TypedExp where
  ppE = pretty
  ppP (TypedExp (str, ty)) = pretty str <+> colon <+> ppP ty