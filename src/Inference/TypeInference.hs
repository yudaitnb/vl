module Inference.TypeInference  where

import Prelude hiding (log)
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import Data.List (foldl, map, elemIndex, sortBy, sortOn, intersect, nub, (\\))
import Control.Monad.State ( forM, forM_, foldM, gets)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.State (evalStateT, runStateT)
import Control.Monad (unless)
import Control.Arrow (second)

import qualified Language.LambdaVL as VL

import Syntax.Common as N

import Syntax.Type as T
import Syntax.Kind
import Syntax.Env
import Syntax.Substitution

-- import Inference.TypeUnification
import Inference.Kinding
import Inference.PatternSynthesis

import Graph
import Util

type TyInfRes =
    ( Type
    -- , TEnv
    , UEnv
    , Subst
    , Constraints
    )

typeOf :: Typable ast => Env' -> ast -> IO TyInfRes
typeOf env ast = evalStateT (infer ast) env

typeOfWithLogs :: Typable ast => Env' -> ast -> IO (TyInfRes, Logs)
typeOfWithLogs env ast = do
  (res, Env' _ _ _ _ l _) <- runStateT (infer ast) env
  return (res, l)

putTyInfLog :: UEnv -> TEnv -> VL.Exp SrcSpanInfo -> TyInfRes -> Env ()
putTyInfLog oldu oldt exp tyInfRes = do
  lc <- gets logc
  let header = ppP "(TypeInf)"
      env = header <+>
        -- ppP oldu <> semicolon <+> -- [UEnv]
        ppP oldt <> vdash
      depth = concatWith (<>) $ replicate lc (ppP "|")
      res = ppP exp <+> ppP "=>" <+> ppP tyInfRes
  putLog $ putDocString $
    depth <>
    env <>
    res
  return ()

class Typable ast where
  infer :: ast -> Env TyInfRes

instance Typable (VL.Exp SrcSpanInfo) where
  infer exp = do
    addLC
    case exp of
      -- ^ ⇒_int, ⇒_char, ⇒_string 
      VL.Lit _ lit -> do
        gamma <- gets tEnv
        sigma <- gets uEnv
        let ty = case lit of
              VL.Char {}   -> tychar
              VL.String {} -> tystring
              VL.Int {}    -> tyint
            result =
              ( ty
              -- , emptyEnv
              , sigma
              , emptySubst
              , emptyConstraints
              )
        putTyInfLog sigma gamma exp result
        return result

      -- ^ ⇒_con
      VL.Con _ qn -> do
        gamma <- gets tEnv
        sigma <- gets uEnv
        case qn of
          Special _ (Cons l) -> do
            ty <- do
              alpha <- genNewTyVar TypeKind
              beta1 <- genNewTyVar LabelsKind
              beta2 <- genNewTyVar LabelsKind
              let arg1 = TyBox beta1 alpha
                  arg2 = TyBox beta2 alpha
              return $ TyFun arg1 (TyFun arg2 (TyList alpha))
            let result = (ty, sigma, emptySubst, emptyConstraints)
            putTyInfLog sigma gamma exp result
            return result
          _ -> error $ "\nGiven special constructor is not supported.\n" ++ show exp

      -- ^ Variables
      VL.Var _ qName
        -- ^ reserved binary operators
        | getName qName `elem` reservedOps -> do
          gamma <- gets tEnv
          sigma <- gets uEnv
          alpha0 <- genNewTyVar LabelsKind
          alpha1 <- genNewTyVar LabelsKind
          sigma' <- gets uEnv
          let (t1,t2,t3)
                | getName qName `elem` ["+", "-", "*", "/"] = 
                  (tyint, tyint, tyint)
                | getName qName `elem` ["&&", "||"] = 
                  (tybool, tybool, tybool)
                | getName qName `elem` ["<" ,"<=", ">", ">=", "==", "/="] =
                  (tyint, tyint, tybool)
              ty = TyFun
                    (TyBox alpha0 t1)
                    (TyFun
                      (TyBox alpha1 t2)
                      t3)
              result =
                ( ty
                -- , emptyEnv
                , sigma'
                , emptySubst
                , emptyConstraints
                )
          putTyInfLog sigma gamma exp result
          return result

        -- ^ ⇒_Lin/Gr?
        | otherwise -> do
          gamma <- gets tEnv
          sigma <- gets uEnv
          case lookupEnv (mkVKFromQN qName) gamma of
            Nothing -> do
              logs <- gets log
              let message = ppP "⇒_Lin/Gr expects a variable" <+>
                            ppP qName <+>
                            ppP "to be in tenv, but it was not found." <> line <>
                            ppP "tenv : " <> ppP gamma <> line <> ppP logs
              error $ putDocString message
            -- ^ ⇒_Lin
            Just ty@(NType tag tya) -> case tag of
              Local -> do
                let result = 
                      ( tya
                      -- , makeEnv [(x, NType Local tya)]
                      , sigma
                      , emptySubst
                      , emptyConstraints
                      )
                putTyInfLog sigma gamma exp result
                return result
              Imported -> do
                -- tya中の全型変数をfreshにする
                -- (tyaFresh, newConstraints) <- fresh tya
                let tyaFresh = tya
                    newConstraints = emptyConstraints
                let result =
                      ( tyaFresh
                      -- , makeEnv [(x, NType Imported tyaFresh)]
                      , sigma
                      , emptySubst
                      , newConstraints
                      )
                putTyInfLog sigma gamma exp result
                return result
            -- ^ ⇒_Gr
            Just ty@(GrType tag tya r) -> case tag of
              Local -> do
                hasLabelsKind r
                let result =
                      ( tya
                      -- , makeEnv [(x, GrType Local tya coeff1)]
                      , sigma
                      , emptySubst
                      , emptyConstraints
                      )
                putTyInfLog sigma gamma exp result
                return result
              Imported -> do
                hasLabelsKind r
                -- tya中の全型変数をfreshにする
                -- (tyaFresh, newConstraints) <- fresh tya
                let tyaFresh = tya
                    newConstraints = emptyConstraints
                let result =
                      ( tyaFresh
                      -- , makeEnv [(x, GrType Imported tyaFresh coeff1)]
                      , sigma
                      , emptySubst
                      , newConstraints
                      )
                putTyInfLog sigma gamma exp result
                return result

      -- ^ ⇒_abs
      VL.Lambda _ p t -> do
        gamma <- gets tEnv
        sigma <- gets uEnv
        alpha <- genNewTyVar TypeKind
        sigma1' <- gets uEnv
        setREnv emptyREnv
        (gamma', sigma2, theta) <- patternSynthesis p alpha
        setUEnv sigma2
        setTEnv $ gamma .+++ gamma' -- シンボルが被った場合は新たに生成した環境で上書き (コア計算とは異なる)
        -- (tyb, delta, sigma3, theta', c1) <- infer t
        (tyb, sigma3, theta', c1) <- infer t
        theta'' <- theta `comp` theta'
        let result =  ( -- typeSubstitution theta (TyFun alpha tyb)
                        typeSubstitution theta'' (TyFun alpha tyb) -- [TODO] ?
                      -- , delta `exclude` gamma'
                      , insertEnv (getName alpha) TypeKind sigma3
                      , theta''
                      , c1)
        putTyInfLog sigma gamma exp result
        return result

      -- ^ ⇒_clet
      VL.CLet _ p t -> do
        gamma <- gets tEnv
        sigma <- gets uEnv
        alpha <- genNewTyVar TypeKind
        sigma1' <- gets uEnv
        setREnv emptyREnv
        (gamma', sigma2, theta) <- patternSynthesis p alpha
        setUEnv sigma2
        setTEnv $ gamma .+++ gamma' -- シンボルが被った場合は新たに生成した環境で上書き (コア計算とは異なる)
        -- (tyb, delta, sigma3, theta', c1) <- infer t
        (tyb, sigma3, theta', c1) <- infer t
        theta'' <- theta `comp` theta'
        let result =  ( -- typeSubstitution theta (TyFun alpha tyb)
                        typeSubstitution theta'' (TyFun alpha tyb) -- [TODO] ?
                      -- , delta `exclude` gamma'
                      , insertEnv (getName alpha) TypeKind sigma3
                      , theta''
                      , c1)
        putTyInfLog sigma gamma exp result
        return result

      -- ^ =>_()
      VL.Tuple _ elms -> do
        gamma <- gets tEnv
        sigma1 <- gets uEnv
        tyInfResLst <- forM elms $ \e -> do
          setTEnv gamma
          infer e
        let resTy = TyTuple $ map (\(t,_,_,_) -> t) tyInfResLst
        sigma' <- gets uEnv
        let sigmaLast = if null tyInfResLst
                          then sigma'
                          else case last tyInfResLst  of (_, sigmaLast, _, _) -> sigmaLast
        thetaSum <- foldM comp emptySubst =<< mapM (\(_,_,thetai,_) -> return thetai) tyInfResLst
        cSum <- foldM (\a b -> return $ a `landC` b) emptyConstraints =<< mapM (\(_,_,_,ci) -> return ci) tyInfResLst
        let result =
              ( resTy
              -- , delta1 .++ delta2
              , sigmaLast
              , thetaSum
              , cSum
              )
        putTyInfLog sigma1 gamma exp result
        return result

      -- ^ =>_[]
      VL.List _ elms -> do
        gamma <- gets tEnv
        sigma1 <- gets uEnv
        tyInfResLst <- forM elms $ \e -> do
          setTEnv gamma
          infer e
        let tys = map (\(t,_,_,_) -> t) tyInfResLst
        resTy <- if not (null tyInfResLst)
          then do
            let tyFirstCase = head tys
            unless (foldr (\ty acc -> ty `tySym` tyFirstCase && acc) True tys) $
              error "All list elements must have a same type."
            return $ TyList tyFirstCase
          else do
            alpha <- genNewTyVar TypeKind
            return $ TyList alpha
        sigma' <- gets uEnv
        let sigmaLast = if null tyInfResLst
                          then sigma'
                          else case last tyInfResLst  of (_, sigmaLast, _, _) -> sigmaLast
        thetaSum <- foldM comp emptySubst =<< mapM (\(_,_,thetai,_) -> return thetai) tyInfResLst
        cSum <- foldM (\a b -> return $ a `landC` b) emptyConstraints =<< mapM (\(_,_,_,ci) -> return ci) tyInfResLst
        let result =
              ( resTy
              -- , delta1 .++ delta2
              , sigmaLast
              , thetaSum
              , cSum
              )
        putTyInfLog sigma1 gamma exp result
        return result

      -- ^ ⇒_app
      VL.App _ t1 t2 -> do
        gamma <- gets tEnv
        sigma1 <- gets uEnv
        -- (tya, delta1, sigma2, theta_1, c1) <- infer t1
        (tya, sigma2, theta_1, c1) <- infer t1
        setTEnv gamma -- [TODO] app規則のtenvを見直すべき
        setUEnv sigma2
        -- (tya', delta2, sigma3, theta_2, c2) <- infer t2
        (tya', sigma3, theta_2, c2) <- infer t2
        beta <- genNewTyVar TypeKind
        let name = getName beta
            sigma4 = insertEnv name TypeKind sigma3
            tyfun = TyFun tya' beta
        setUEnv sigma4
        theta_3 <- typeUnification tya tyfun
        theta_4 <- theta_1 `comp` theta_2 >>= comp theta_3
        let beta' = typeSubstitution theta_4 beta
            result =
              ( beta'
              -- , delta1 .++ delta2
              , sigma3
              , theta_4
              , c1 `landC` c2
              )
        putTyInfLog sigma1 gamma exp result
        return result

      -- ^ ⇒_case
      VL.Case _ t alts -> do
        -- print . putDocString . ppP <$> gets tEnv
        gamma <- gets tEnv
        sigma1 <- gets uEnv
        (tya, _, theta0, c0) <- infer t
        resAlts <- forM alts $ \(VL.Alt _ pi ti) -> do
          setTEnv gamma
          alpha <- genNewTyVar LabelsKind
          let name = getName alpha
          setREnv emptyREnv
          (deltai, sigmai, thetai) <- patternSynthesis pi tya
          setUEnv sigmai
          setTEnv $ gamma .+++ deltai -- シンボルが被った場合はdeltaiに新たに束縛した変数でgammaの環境を上書き (コア計算とは異なる)
          -- te <- gets tEnv
          -- liftIO $ print . putDocString . ppP $ te
          (tyb, sigmai', thetai', ci') <- infer ti
          thetai'' <- thetai `comp` thetai'
          return (tyb, sigmai', thetai'', ci')
        let (tyb, _, _, _) = head resAlts
            (_, sigmaLast, _, _) = last resAlts
            tys = map (\(ty,_,_,_) -> ty) resAlts
        thetaAlts <- foldM (\acc ty -> (acc `comp`) =<< typeUnification ty (head tys)) emptySubst (tail tys)
        -- unless (foldr (\ty acc -> ty == tyb && acc) True tys) $
        --   error $ "\nAll case altanatives must have the same type.\n" ++ putDocString (concatWith (surround line) $ map ppP tys)
        thetaSum <- (thetaAlts `comp`) =<< foldM comp theta0 =<< mapM (\(_,_,thetai,_) -> return thetai) resAlts
        cSum <- foldM (\a b -> return $ a `landC` b) c0 =<< mapM (\(_,_,_,ci) -> return ci) resAlts
        let result =  ( typeSubstitution thetaSum tyb
                      -- , delta `exclude` gamma'
                      , sigmaLast
                      , thetaSum
                      , cSum)
        putTyInfLog sigma1 gamma exp result
        return result

      -- ^ ⇒_pr
      VL.Pr _ t -> do
        gamma <- gets tEnv
        sigma1 <- gets uEnv
        let gamma' = gradeTEnv (filterEnvBy (VL.freeVars t) gamma)
            gamma'_dont_care_inside_vext = gradeTEnv (filterEnvBy (VL.freeVars' t) gamma)
        -- [TODO] 要検討
        -- unversion t (vext)のt中の自由変は2で扱う
        -- 1. この場の制約生成において無視し、環境中のその変数はgradeしておく
        -- 2. この場の環境生成において無視し、環境中のその変数はgradeしない
        setTEnv gamma'
        -- (tya, delta, sigma2, theta, c1) <- infer t
        (tya, sigma2, theta, c1) <- infer t
        alpha <- genNewTyVar LabelsKind
        let c2 = genConstraint alpha gamma'_dont_care_inside_vext
            name = getName alpha
            result =  ( TyBox alpha tya
                      -- , alpha .** delta
                      , insertEnv name LabelsKind sigma2
                      , theta,
                      c1 `landC` c2
                      )
        putTyInfLog sigma1 gamma exp result
        return result

      -- ^ ⇒_res
      VL.VRes _ ls t -> do
        gamma <- gets tEnv
        sigma1 <- gets uEnv
        let r = TyLabels ls
        hasLabelsKind r
        let -- gamma' = r .** gradeTEnv (filterEnvBy (VL.freeVars t) gamma) -- [TODO] tの型付けでrが使用可能であると見做す？これは何かおかしいような気もする。
            gamma' = gradeTEnv (filterEnvBy (VL.freeVars t) gamma)
            gamma'_dont_care_inside_vext = gradeTEnv (filterEnvBy (VL.freeVars' t) gamma)
        setTEnv gamma'
        -- (tya, delta, sigma2, theta, c1) <- infer t
        (tya, sigma2, theta, c1) <- infer t
        let c2 = genConstraintBy r gamma'_dont_care_inside_vext
            result =  ( tya
                      -- , delta -- -- , r .** delta -- tの型付けがどうなっていようと、tの型付けではrを使用としたと見做す
                      , sigma2
                      , theta,
                      c1 `landC` c2
                      )
        putTyInfLog sigma1 gamma exp result
        return result

      -- ^ ⇒_ext
      VL.VExt _ t -> do
        gamma <- gets tEnv
        sigma <- gets uEnv
        -- (tya, delta, sigma', theta', c1) <- infer t
        (tya, sigma', theta', c1) <- infer t
        let tyb = case tya of (TyBox _ tya') -> tya'
                              _              -> error $ putDocString $ ppP "=>_Ext rule expects tya to be graded type, but it has " <> ppP tya
        let result =  ( tyb
                      -- , delta
                      , sigma
                      , theta'
                      , c1)
        putTyInfLog sigma gamma exp result
        return result

      _ -> error "The function `infer` is not defined for a given expression."

data TypedExp = TypedExp
  { name         :: VarKey -- シンボル名
  , inferredType :: Type        -- 推論された型
  , constraints  :: Constraints -- 推論された型が満たすべき制約
  , uenv         :: UEnv        -- この型付けで生成された型変数
  }
  deriving (Show)

-- [TODO] 難読。再帰も不明瞭。書き直す
getInterface :: TEnv -> UEnv -> Int -> VL.Module SrcSpanInfo -> IO ([(TypedExp, Logs)], Int)
getInterface importedTEnv importedUEnv initC (VL.Module _ mmh _ _ decls) = do
  let mn = getName $ fromMaybe (error "getInterface : undifined error") mmh
      initEnv = Env' initC importedTEnv importedUEnv emptyREnv emptyLogs initLC
  (typedExpLogs, env) <- runStateT
                            (getInterface' mn $ reverse $ tsortDecls decls)
                            initEnv
  return (rearrange typedExpLogs, counter env)
  where
    getInterface' :: ModName -> [VL.Decl SrcSpanInfo] -> Env [(TypedExp, Logs)]
    getInterface'  mn []                             = return []
    getInterface'  mn ((VL.PatBind _ pat exp):decls) = do
      decls' <- getInterface' mn decls

      -- 次の宣言の型検査の前処理
      initializeEnv
      setTEnv importedTEnv
      setUEnv importedUEnv

      -- 環境にこれまで生成した束縛・制約を追加
      -- TypedExp中の型はpromote済みなので、モジュール内の型検査の際にはTyBoxの中の型を取り出す
      let prevTyExps = map fst decls'
      forM_ prevTyExps $ \(TypedExp vk ty@(TyBox c tyin) _ _) -> do
        alpha <- genNewTyVar LabelsKind
        putTEnv vk (GrType Local tyin alpha)

      -- 全てのトップレベルシンボルは再帰関数扱い
      alpha <- genNewTyVar TypeKind
      beta  <- genNewTyVar LabelsKind
      let qkTarget = QVar mn (getName pat)
      putTEnv qkTarget (GrType Local alpha beta)

      -- 対象の宣言の型検査 with promotion
      (ty, uenv, sub, con) <- infer $ VL.Pr (VL.ann exp) exp
      let con' = constraintsSubstitution sub con
      l <- gets (reverseLogs . log)
      return $
        (TypedExp qkTarget ty con' uenv, l) : decls'

    -- 元の宣言の順番に並び替える
    rearrange :: [(TypedExp, Logs)] -> [(TypedExp, Logs)]
    rearrange = sortOn (\(typedExp, l) -> getName (name typedExp) `elemIndex` map getName decls)


-- ^ declsを変数依存関係の下流から上流にtsortする
-- ^ 事前条件:
-- ^ (1) 相互再帰(有向サイクル)を含まない
tsortDecls :: PrettyAST l => [VL.Decl l] -> [VL.Decl l]
tsortDecls decls = tsortBy g getName decls
  where
    nodes = map (\(VL.PatBind _ p _) -> getName p) decls
    edges = distEdges $ map (\(VL.PatBind _ pat exp) -> (getName pat, map getVN (VL.freeVars exp))) decls
    g = Graph (nodes, edges)

aggregateConstraints :: [TypedExp] -> Constraints
aggregateConstraints [] = CTop
aggregateConstraints ((TypedExp _ _ c _) : rst) = CAnd c $ aggregateConstraints rst

------------------------------

instance PrettyAST TyInfRes where
  -- ppE (ty, tenv, uenv, subst, c) = let sep = ppE "; " in
  --   ppE ty
  --     <> sep <> ppE tenv
  --     -- <> sep <> ppE uenv -- [UEnv]
  --     <> sep <> ppE subst
  --     <> sep <> ppE c
  ppE (ty, uenv, subst, c) = let sep = ppE "; " in
    ppE ty
      <> sep <> ppE subst
      <> sep <> ppE c
  -- ppP (ty, tenv, uenv, subst, c) = parens $
  --   ppP ty
  --     <> semicolon <+> ppP tenv
  --     -- <> semicolon <+> ppP uenv -- [UEnv]
  --     <> semicolon <+> ppP subst
  --     <> semicolon <+> ppP c
  ppP (ty, uenv, subst, c) = parens $
    ppP ty
      <> semicolon <+> ppP subst
      <> semicolon <+> ppP c

instance PrettyAST TypedExp where
  ppE (TypedExp s ty c uenv) =
        ppE s <+> colon <+> ppE ty
    -- <+> ppE "|" <+> ppE c
    -- <+> ppE "|" <+> ppE uenv
  ppP (TypedExp s ty c uenv) =
        ppP s <+> colon <+> ppP ty
    -- <+> ppP "|" <+> ppP c
    -- <+> ppP "|" <+> ppP uenv

instance PrettyAST [TypedExp] where
  ppE lst = concatWith (surround line) $ map ppE lst
  ppP lst = concatWith (surround line) $ map ppP lst