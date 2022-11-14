{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Syntax.Substitution where

import Prelude hiding (log)
import Data.Maybe (fromMaybe)
import Data.List (sortOn, intersect, elemIndex)

import Language.LambdaVL
import Syntax.Type
import Syntax.Env
import Inference.Kinding

import Graph

import Util
import Control.Arrow (second)
import Control.Monad.State (gets)
import Control.Monad.Trans.State (evalStateT)
import Control.Monad (forM,foldM)

newtype Subst = Subst { subst :: [(String, Type)] }

instance Semigroup Subst where
  Subst a <> Subst b = Subst (a ++ b)

instance Semigroup Subst => Monoid Subst where
  mempty = emptySubst  

instance HasVar Subst where
  freeVars s = case s of
    Subst []  -> []
    Subst ((s,ty):lst) ->  "s" : freeVars ty ++ freeVars (Subst lst)
  freeVars' s = case s of
    Subst []  -> []
    Subst ((s,ty):lst) ->  "s" : freeVars' ty ++ freeVars' (Subst lst)
  vars s = case s of
    Subst []  -> []
    Subst ((s,ty):lst) ->  "s" : vars ty ++ vars (Subst lst)

emptySubst :: Subst
emptySubst = Subst []

makeSubst :: String -> Type -> Subst
makeSubst k v = Subst [(k, v)]

findSubst :: String -> Subst -> Maybe Type
findSubst str sub = lookup str (subst sub)

(\\) :: Subst -> String -> Subst
(\\) s str = Subst $ dropWhile (\(k,_) -> k == str) (subst s)

putSubstCompLog :: UEnv -> Subst -> Subst -> Subst -> Env ()
putSubstCompLog oldu s1 s2 s3 = do
  lc <- gets logc
  let header = ppP "(CompSub)"
      env = header <+> 
        -- ppP oldu <+> -- [UEnv]
        vdash
      depth = concatWith (<>) $ replicate lc (ppP "|")
      res = ppP s1 <+> ppP "⊎" <+> ppP s2 <+> ppP "=>" <+> ppP s3
  putLog $ putDocString $
    depth <>
    env <>
    res
  return ()

-- 最般単一化子になるよう2つの代入を合成
-- 前提：s1 s2は以下のような代入ではない
-- - 解決の結果循環参照を含む代入(トポロジカルソートの前提を満たさない)
--   o: (a0 ↦ a1 -> a2) `comp` (a1 ↦ a2) => (a0 ↦ a2 -> a2, a1 ↦ a2))
--   x: (a0 ↦ a1 -> a2) `comp` (a1 ↦ a0) => fail
comp :: Subst -> Subst -> Env Subst
comp s1 s2 = do
  uenv <- gets uEnv
  s2 <- rmSameTyVar s1 s2
  let nodes = varsInEnv uenv -- [TODO] 普通の関数にしたい。UEnvを参照する必要はないはず
      edges = distEdges $ map (second freeVars) (subst s2)
      g = Graph (nodes, edges)
      result = normalize $ Subst $ tsortBy g fst (subst s2)
  -- putSubstCompLog uenv s1 s2 result
  return result

-- 同一domainの代入を除去
-- Granule論文のcomp
rmSameTyVar :: Subst -> Subst -> Env Subst
rmSameTyVar (Subst []) theta2 = return theta2
rmSameTyVar (Subst ((alpha,tya):theta1)) theta2 =
  case findSubst alpha theta2 of
    Just tyb -> do
      theta <- typeUnification tya tyb
      theta12 <- Subst theta1 `comp` (theta2 \\ alpha)
      theta12' <- theta12 `comp` theta
      return $ Subst $ (alpha, typeSubstitution theta tya) : subst theta12'
    Nothing -> do
      theta12 <- Subst theta1 `comp` theta2
      return $ Subst $ (alpha, tya) : subst theta12

-- 解を生成
normalize :: Subst -> Subst
normalize (Subst [])          = emptySubst
normalize (Subst ((k,v):lst)) = Subst $ (k,v) : map (second (typeSubstitution (Subst [(k,v)]))) (subst $ normalize $ Subst lst)

isCompatible :: Subst -> Env Bool
isCompatible (Subst []) = return True
isCompatible (Subst ((alpha, tya):theta)) = do
  sigma <- gets uEnv
  case lookupEnv alpha sigma of
    Nothing    -> return False
    Just kappa -> do
      b1 <- isCompatible $ Subst theta
      setUEnv (exclude sigma (makeEnv [(alpha, kappa)]))
      kappa' <- kind tya
      return $ kappa == kappa'

--------------------------

typeSubstitution :: Subst -> Type -> Type
typeSubstitution s_table ty =
  let tysubst = typeSubstitution s_table in
  case ty of
    TyCon qName -> ty
    TyBottom    -> ty
    TyLabels _  -> ty
    TyVar name  -> fromMaybe ty (findSubst (getName name) s_table)
    TyTuple ts  -> TyTuple $ map tysubst ts
    TyList t    -> TyList $ tysubst t
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
    -- CSubset c1 c2 ->
    --   let c1' = tysubst c1
    --       c2' = tysubst c2
    --   in CSubset c1' c2'

constraintsSubstitution :: Subst -> Constraints -> Constraints
constraintsSubstitution s_table cs =
  let tySubst = typeSubstitution s_table
      csSubst = constraintsSubstitution s_table in
  case cs of
    CTop          -> CTop
    CSubset c1 c2 -> CSubset (tySubst c1) (tySubst c2)
    CAnd c1 c2    -> CAnd (csSubst c1) (csSubst c2)
    COr c1 c2     -> COr (csSubst c1) (csSubst c2)

----------------------------

putUnifyLog :: UEnv -> Type -> Type -> Subst -> Env ()
putUnifyLog oldu ty1 ty2 subst = do
  lc <- gets logc
  let header = ppP "(TyUnify)"
      env = header <+>
        -- ppP oldu <> [UEnv]
        vdash
      depth = concatWith (<>) $ replicate lc (ppP "|")
      res = ppP ty1 <+> ppP "~" <+> ppP ty2 <+> ppP "▷" <+> ppP subst
  putLog $ putDocString $ 
    depth <>
    env <>
    res
  return ()

typeUnification :: Type -> Type -> Env Subst
typeUnification ty1 ty2 = do
  addLC
  sigma <- gets uEnv
  case (ty1, ty2) of
    -- U_→
    (TyFun a b, TyFun a' b') -> do
      theta_1 <- typeUnification a' a
      theta_2 <- typeUnification (typeSubstitution theta_1 b) (typeSubstitution theta_1 b')
      result <- comp theta_1 theta_2
      putUnifyLog sigma ty1 ty2 result
      return result
    
    -- U_()
    (TyTuple tys, TyTuple tys') -> do
      thetas <- forM (zip tys tys') $ \(a, a') -> do
        typeUnification a a' -- [TODO] 以下のように、前の型の生成した代入を次の型引数に適用する必要があるか？
        -- theta_2 <- typeUnification (typeSubstitution theta_1 b) (typeSubstitution theta_1 b')
      result <- foldM comp emptySubst thetas
      putUnifyLog sigma ty1 ty2 result
      return result

    -- U_[]
    (TyList a, TyList a') -> do
      theta <- typeUnification a a'
      let result = theta
      putUnifyLog sigma ty1 ty2 result
      return result

    -- U_box
    (TyBox r a, TyBox r' a') -> do
      theta_1 <- typeUnification a a'
      theta_2 <- typeUnification (typeSubstitution theta_1 r) (typeSubstitution theta_1 r') -- [TODO] typeSubstitution不要かも
      result <- comp theta_1 theta_2
      putUnifyLog sigma ty1 ty2 result
      return result
    -- U_var=, U_var∃
    (TyVar alpha, ty2) -> do
      let result = if ty1 == ty2
                      then emptySubst                    -- [TODO] well-definedness
                      else makeSubst (getName alpha) ty2 -- [TODO] well-definedness
      putUnifyLog sigma ty1 ty2 result
      return result
    (ty1, TyVar alpha) -> do
      let result = if ty1 == ty2
                      then emptySubst                    -- [TODO] well-definedness
                      else makeSubst (getName alpha) ty1 -- [TODO] well-definedness
      putUnifyLog sigma ty1 ty2 result
      return result
    -- U_=
    (ty1, ty2) ->
      if ty1 == ty2
        then do
          let result = emptySubst -- [TODO] well-definedness
          putUnifyLog sigma ty1 ty2 result
          return result
        else do
          logs <- gets log
          error $ putDocString $
            line <> ppP "[ERROR] typeUnification: ty1 and ty2 cannot be unified" <> line <>
            ppP "ty1 :" <+> ppP ty1 <> line <> 
            ppP "ty2 :" <+> ppP ty2 <> line <> ppP logs

unify :: Type -> Type -> IO Subst
unify t1 t2 = evalStateT (typeUnification t1 t2) (Env' initCounter emptyEnv emptyEnv emptyREnv emptyLogs initLC)

--------------------------

instance PrettyAST Subst where
  ppE s = parens $ concatWith (surround $ comma <> space) $ map (\(k,v) -> ppE k <> ppE " ↦ " <> ppP v) (subst s) -- [TODO]
  ppP s = parens $ concatWith (surround $ comma <> space) $ map (\(k,v) -> ppP k <> ppP " ↦ " <> ppP v) (subst s)