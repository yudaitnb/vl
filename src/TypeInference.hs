{-# LANGUAGE TypeFamilies #-}
module TypeInference (getInterface) where

import qualified Syntax.LambdaVL as VL
import Data.Maybe (fromMaybe)
import Control.Monad.State
import Data.Map
import Data.List (foldl)

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

data InferenceEnv' = InferenceEnv'
  { counter :: Int -- 単一化型変数の累積数
  , tEnv :: TEnv   -- 型付け環境 / [x:A]
  , uEnv :: UEnv   -- 単一化用型変数環境 / [X:κ]
  }
type Env a = State InferenceEnv' a

prefixNewTyVar :: String
prefixNewTyVar = "_tyvar_"

getCounter :: Env Int
getCounter = state $ \env@(InferenceEnv' c _ _) -> (c, env)

getTEnv :: Env TEnv
getTEnv = state $ \env@(InferenceEnv' _ t _) -> (t, env)

getUEnv :: Env UEnv
getUEnv = state $ \env@(InferenceEnv' _ _ u) -> (u, env)

setTEnv :: TEnv -> Env ()
setTEnv tenv = state $ \(InferenceEnv' c _ u) -> ((), InferenceEnv' c tenv u)

setUEnv :: UEnv -> Env ()
setUEnv uenv = state $ \(InferenceEnv' c t _) -> ((), InferenceEnv' c t uenv)

addTEnv :: String -> EnvType -> Env ()
addTEnv str envType = state $ \(InferenceEnv' c t u) ->
  let tenv' = insert str envType t in
  return $ InferenceEnv' c tenv' u

addUEnv :: String -> Kind -> Env ()
addUEnv str kind = state $ \(InferenceEnv' c t u) ->
  let uenv' = insert str kind u in
  return $ InferenceEnv' c t uenv'

genNewTyVar :: Kind -> Env Type
genNewTyVar kind = state $ \(InferenceEnv' c t u) ->
  let strvar' = prefixNewTyVar ++ show c
      counter' = c + 1
      uenv' = insert strvar' kind u
      tyvar = TyVar (Ident strvar')
  in (tyvar, InferenceEnv' counter' t uenv')



typeOf :: Typable ast => Int -> TEnv -> UEnv -> ast -> (Type, SubstMap)
typeOf c t u ast = evalState (infer ast) (InferenceEnv' c t u)

class Typable ast where
  infer :: ast -> Env (Type, SubstMap)

instance Typable (VL.Rhs l) where
  infer rhs = case rhs of
    VL.UnGuardedRhs _ exp -> infer exp

instance Typable (VL.Exp l) where
  infer exp = case exp of
    -- ^ ⇒_int
    VL.Lit _ (VL.Char {})   -> return (TyCon (UnQual (Ident "Char")), empty)
    VL.Lit _ (VL.String {}) -> return (TyCon (UnQual (Ident "String")), empty)
    VL.Lit _ (VL.Int {})    -> return (TyCon (UnQual (Ident "Int")), empty)
    -- ^ ⇒_Lin, ⇒_Gr
    VL.Var _ qName -> do
      tenv_old <- getTEnv
      let str = VL.getName qName
      case lookup str tenv_old of
        Nothing  -> error $ "[Exp - infer@TypeInference.hs] The variable " ++ str ++ " is not in the typing context."
        Just res -> case res of
          NType  ty   -> return (ty, empty)
          GrType ty c -> do
            uenv <- getUEnv
            isLabelsKind uenv c
            setTEnv $ Syntax.Env.addTEnv str (GrType ty coeff1) emptyTEnv
            return (ty, empty)
    -- ^ ⇒_app
    VL.App _ e1 e2 -> do
      (funTy, theta_1) <- infer e1
      delta_1 <- getTEnv
      (tya', theta_2) <- infer e2
      case funTy of
        TyFun tya tyb -> do
          uenv <- getUEnv
          let theta_3 = unify uenv tya tya'
              theta_4 = union theta_1 theta_2 `union` theta_3
              tyb' = typeSubstitution theta_4 tyb
          delta_2 <- getTEnv
          setTEnv $ delta_1 .++ delta_2
          return (tyb', theta_4)
        _ -> error $ "[Exp - infer@TypeInference.hs] The variable " ++ show funTy ++ " is not a function type."
    -- ^ ⇒_pr
    VL.Pr _ t -> do
      gamma <- getTEnv
      sigma1 <- getUEnv
      let gamma' = gradeCtx (filterTEnv gamma (VL.freeVars t))
      setTEnv gamma'
      (tya, theta) <- infer t
      alpha <- genNewTyVar LabelsKind
      delta <- getTEnv
      let delta' = alpha .** delta
      setTEnv delta'
      return (TyBox alpha tya, theta)
    -- ^ ⇒_abs
    lam@(VL.Lambda _ (p:nil) t) -> do
      gamma <- getTEnv
      alpha <- genNewTyVar TypeKind
      sigma1' <- getUEnv
      c <- getCounter
      let (gamma', theta) = patSynth c sigma1' emptyREnv p alpha
      let gamma'' = gamma .++ gamma' -- [TODO] , == .++  でいいの?
      setTEnv gamma''
      (tyb, theta') <- infer t
      let tyres = typeSubstitution theta (TyFun alpha tyb)
          theta'' = theta `comp` theta' -- [TODO] tenv, uenv
      return (tyres, theta'')
    lam@(VL.Lambda {}) -> do
      let lam' = VL.decon lam -- \x y. ... ==> \x. \y. ... 
      infer lam'
    -- InfixApp
    VL.InfixApp l e1 (VL.QVarOp l2 qName) e2 -> do
      infer $ VL.App l (VL.App l (VL.Var l2 qName) e1) e2
    _              -> error "[Exp - infer@TypeInference.hs] The function `infer` is not defined for a given expression."

newtype TypedExp = TypedExp [(String, Type)]
instance Pretty TypedExp where
  pretty (TypedExp []) = pretty ""
  pretty (TypedExp ((str,ty):rst)) =
       pretty str <+> pretty ":" <+> pretty ty <> line
    <> pretty (TypedExp rst)

typeDecl :: Show l => VL.Decl l -> TypedExp
typeDecl (VL.FunBind l1 []) = TypedExp [] 
typeDecl (VL.FunBind l1 ((VL.Match l2 name pats (VL.UnGuardedRhs _ exp) maybeBinds):ms)) = 
  let
    str = VL.getName name
    match' = VL.Lambda l2 pats exp
    (ty, subst) = typeOf 0 basicTEnv emptyUEnv match'
    TypedExp res = typeDecl (VL.FunBind l1 ms) 
  in TypedExp ((str,ty):res) -- [TODO] maybeBinds
typeDecl (VL.PatBind _ pat@(VL.PVar _ name) rhs maybeBinds) = 
  let str = VL.getName name
      (ty, subst) = typeOf 0 basicTEnv emptyUEnv rhs
  in TypedExp [(str, ty)] -- [TODO] maybeBinds
typeDecl decl = error $ "[TODO] typeDecl is not defined for `bind`:" ++ show decl

getInterface :: Show l =>  VL.Module l -> TypedExp
getInterface (VL.Module _ _ _ decls) =
  Data.List.foldl
    (\acc decl -> case (acc, typeDecl decl) of
      (TypedExp acc', TypedExp decl') -> TypedExp (acc' ++ decl'))
    (TypedExp [])
    decls
