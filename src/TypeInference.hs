{-# LANGUAGE TypeFamilies #-}
module TypeInference  where

import qualified Syntax.LambdaVL as VL
import Data.Maybe (fromMaybe)
import Control.Monad.State
import Data.List (foldl, map)

import Syntax.Type
import Syntax.Kind
import Syntax.Env
import Syntax.Substitution
import TypeUnification
import Kinding
import Prelude hiding (lookup)
import TypeSubstitution
import PatternSynthesis
import Prettyprinter

typeOf :: Typable ast => Env' ->  ast -> (Type, TEnv, UEnv, SubstMap)
typeOf env' ast = evalState (infer ast) env'

class Typable ast where
  infer :: ast -> Env (Type, TEnv, UEnv, SubstMap)

instance Show l => Typable (VL.Exp l) where
  infer exp = case exp of
    -- ^ ⇒_int
    VL.Lit _ (VL.Char {}) -> do
      gamma <- getTEnv
      sigma <- getUEnv
      let tychar = TyCon (UnQual (Ident "Char"))
      return (tychar, emptyEnv, sigma, emptySubst)
    VL.Lit _ (VL.String {}) -> do
      gamma <- getTEnv
      sigma <- getUEnv
      let tystr = TyCon (UnQual (Ident "String"))
      return (tystr, emptyEnv, sigma, emptySubst)
    VL.Lit _ (VL.Int {}) -> do
      gamma <- getTEnv
      sigma <- getUEnv
      let tystr = TyCon (UnQual (Ident "Int"))
      return (tystr, emptyEnv, sigma, emptySubst)

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
        Just ty@(NType tya) ->
          return (tya, makeEnv [(x, ty)], sigma, emptySubst)
        -- ^ ⇒_Gr
        Just ty@(GrType tya r) -> do
          hasLabelsKind r
          return (tya, makeEnv [(x, GrType tya coeff1)], sigma, emptySubst)

    -- ^ ⇒_app
    VL.App _ e1 e2 -> do
      gamma <- getTEnv
      sigma1 <- getUEnv
      (funTy, delta1, sigma2, theta_1) <- infer e1
      case funTy of
        TyFun tya tyb -> do
          setUEnv sigma2
          (tya', delta2, sigma3, theta_2) <- infer e2
          setUEnv sigma3
          theta_3 <- typeUnification tya tya'
          let theta_4 = (theta_1 `comp` theta_2) `comp` theta_3
              tyb' = typeSubstitution theta_4 tyb
          return (tyb', delta1 .++ delta2, sigma3, theta_4)
        _ ->
          let message = "⇒_app expects a term have function type, but it doesn't.\n"
                        ++ "         e1:" ++ show e1 ++ "\n"
                        ++ " type of e1:" ++ show funTy
          in error message

    -- ^ ⇒_pr
    VL.Pr _ t -> do
      gamma <- getTEnv
      sigma1 <- getUEnv
      let gamma' = gradeTEnv (filterEnvBy (VL.freeVars t) gamma)
      setTEnv gamma'
      (tya, delta, sigma2, theta) <- infer t
      alpha <- genNewTyVar LabelsKind
      let name = getName alpha
      return
        ( TyBox alpha tya
        , alpha .** delta
        , insertEnv name LabelsKind sigma2
        , theta)

    -- ^ ⇒_abs
    lam@(VL.Lambda _ p t) -> do
      gamma <- getTEnv
      alpha <- genNewTyVar LabelsKind
      let name = getName alpha
      sigma1' <- getUEnv
      setREnv emptyREnv
      (gamma', sigma2, theta) <- patternSynthesis p alpha
      setUEnv sigma2
      setTEnv $ gamma .++ gamma' -- [TODO] , == .++  でいいの?
      (tyb, delta, sigma3, theta') <- infer t
      return
        (typeSubstitution theta (TyFun alpha tyb)
        , delta `exclude` gamma'
        , insertEnv name LabelsKind sigma3
        , theta `comp` theta')
    _ -> error "The function `infer` is not defined for a given expression."

newtype TypedExp = TypedExp (String, Type)
instance Pretty TypedExp where
  pretty (TypedExp (str,ty)) = pretty str <+> pretty ":" <+> pretty ty <> line

getInterface :: Show l =>  VL.Module l -> [TypedExp]
getInterface (VL.Module _ _ _ decls) = Data.List.map typeDecl decls
  where
    typeDecl :: Show l => VL.Decl l -> TypedExp
    typeDecl (VL.PatBind _ pat exp) = 
      let name = getName pat
          env = Env' 0 basicTEnv emptyEnv emptyREnv
          (ty, tenv, uenv, subst) = typeOf env exp
      in TypedExp (name, ty)