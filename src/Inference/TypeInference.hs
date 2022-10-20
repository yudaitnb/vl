module Inference.TypeInference  where

import Data.Maybe (fromMaybe)
import Data.Map (Map)
import Data.List (foldl, map, elemIndex, sortBy, sortOn, intersect, nub, (\\))
import Control.Monad.State ( forM_, evalState, runState, gets )
import Control.Arrow (second)

import qualified Syntax.LambdaVL as VL
import qualified Syntax.Name as N
import Syntax.Type
import Syntax.Kind
import Syntax.Env
import Syntax.Substitution
import Syntax.SrcLoc

-- import Inference.TypeUnification
import Inference.Kinding
import Inference.PatternSynthesis

import Graph
import Util
import Prelude hiding (log)

type TyInfRes =
    ( Type
    -- , TEnv
    , UEnv
    , Subst
    , Constraints
    )

typeOf :: Typable ast => Env' -> ast -> TyInfRes
typeOf env ast = evalState (infer ast) env

typeOfWithLogs :: Typable ast => Env' -> ast -> (TyInfRes, Logs)
typeOfWithLogs env ast =
  let (res, Env' _ _ _ _ l _) = runState (infer ast) env
  in (res, l)

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
      -- ^ ⇒_int
      VL.Lit _ (VL.Char {}) -> do
        gamma <- gets tEnv
        sigma <- gets uEnv
        let tychar = TyCon (UnQual (Ident "Char"))
            result =
              ( tychar
              -- , emptyEnv
              , sigma
              , emptySubst
              , emptyConstraints
              )
        putTyInfLog sigma gamma exp result
        return result
      VL.Lit _ (VL.String {}) -> do
        gamma <- gets tEnv
        sigma <- gets uEnv
        let tystr = TyCon (UnQual (Ident "String"))
            result =
              ( tystr
              -- , emptyEnv
              , sigma
              , emptySubst
              , emptyConstraints
              )
        putTyInfLog sigma gamma exp result
        return result
      VL.Lit _ (VL.Int {}) -> do
        gamma <- gets tEnv
        sigma <- gets uEnv
        let tyint = TyCon (UnQual (Ident "Int"))
            result =
              ( tyint
              -- , emptyEnv
              , sigma
              , emptySubst
              , emptyConstraints
              )
        putTyInfLog sigma gamma exp result
        return result

      -- ^ Variables
      VL.Var _ qName
        -- ^ +, -, *, /
        | getName qName `elem` ["+", "-", "*", "/"] -> do
          gamma <- gets tEnv
          sigma <- gets uEnv
          alpha0 <- genNewTyVar LabelsKind
          alpha1 <- genNewTyVar LabelsKind
          sigma' <- gets uEnv
          let intToIntToInt = TyFun (TyBox alpha0 tyint)
                                    (TyFun (TyBox alpha1 tyint) tyint)
              result =
                ( intToIntToInt
                -- , emptyEnv
                , sigma'
                , emptySubst
                , emptyConstraints
                )
          putTyInfLog sigma gamma exp result
          return result

        -- ^ &&, ||
        | getName qName `elem` ["&&", "||"] -> do
          gamma <- gets tEnv
          sigma <- gets uEnv
          alpha0 <- genNewTyVar LabelsKind
          alpha1 <- genNewTyVar LabelsKind
          sigma' <- gets uEnv
          let boolToBoolToBool = TyFun (TyBox alpha0 tybool)
                                       (TyFun (TyBox alpha1 tybool) tybool)
              result =
                ( boolToBoolToBool
                -- , emptyEnv
                , sigma'
                , emptySubst
                , emptyConstraints
                )
          putTyInfLog sigma gamma exp result
          return result

        -- ^ < ,<=, >, >=, ==, /=
        | getName qName `elem` ["<" ,"<=", ">", ">=", "==", "/="] -> do
          gamma <- gets tEnv
          sigma <- gets uEnv
          alpha0 <- genNewTyVar LabelsKind
          alpha1 <- genNewTyVar LabelsKind
          sigma' <- gets uEnv
          let intTointToBool = TyFun (TyBox alpha0 tyint)
                                     (TyFun (TyBox alpha1 tyint) tybool)
              result =
                ( intTointToBool
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
          let x = getName qName
          case lookupEnv x gamma of
            Nothing -> do
              logs <- gets log
              let message = "⇒_Lin/Gr expects a variable "
                            ++ show x
                            ++ " to be in tenv, but it was not found.\n"
                            ++ "tenv : " ++ putDocString (ppP gamma)
                            ++ putDocString (ppP logs)
              error message
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

      -- ^ ⇒_pr
      VL.Pr _ t -> do
        gamma <- gets tEnv
        sigma1 <- gets uEnv
        let gamma' = gradeTEnv (filterEnvBy (VL.freeVars t) gamma)
            gamma'_dont_care_inside_vext = gradeTEnv (filterEnvBy (VL.freeVars' t) gamma)
        -- [TODO] 要検討
        -- unversion t (vext)のt中の自由変数は下のいずれかで扱うべきか？
        -- 1. この場の制約生成において無視し、環境中のその変数はgradeしておく or
        -- 2. この場の環境生成において無視し、環境中のその変数はgradeしない or
        -- 3. 1,2のどちらでも変わらない？
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

      -- ^ ⇒_abs
      VL.Lambda _ p t -> do
        gamma <- gets tEnv
        sigma <- gets uEnv
        alpha <- genNewTyVar LabelsKind
        let name = getName alpha
        sigma1' <- gets uEnv
        setREnv emptyREnv
        (gamma', sigma2, theta) <- patternSynthesis p alpha
        setUEnv sigma2
        setTEnv $ gamma .++. gamma'
        -- (tyb, delta, sigma3, theta', c1) <- infer t
        (tyb, sigma3, theta', c1) <- infer t
        theta'' <- theta `comp` theta'
        let result =  ( -- typeSubstitution theta (TyFun alpha tyb)
                        typeSubstitution theta'' (TyFun alpha tyb) -- [TODO] ?
                      -- , delta `exclude` gamma'
                      , insertEnv name LabelsKind sigma3
                      , theta''
                      , c1)
        putTyInfLog sigma gamma exp result
        return result

      -- ^ ⇒_res
      VL.VRes _ ls t -> do
        gamma <- gets tEnv
        sigma1 <- gets uEnv
        let r = TyLabels ls
        hasLabelsKind r
        let gamma' = r .** gradeTEnv (filterEnvBy (VL.freeVars t) gamma) -- [TODO] tの型付けでrが使用可能であると見做す？これは何かおかしいような気もする。
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


-- 型中の型変数を全て新しい型変数にリネームする
-- fresh :: Type -> Env (Type, Constraints)
-- fresh ty = do
--   uenv <- gets uEnv
--   case ty of
--     TyVar name -> do
--       k <- kind ty
--       newtv <- genNewTyVar k
--       return (newtv, [CSubset newtv ty])
--     TyCon qn -> return (TyCon qn, [])
--     TyFun t1 t2 -> do
--       (t1', cs1) <- fresh t1
--       (t2', cs2) <- fresh t2
--       return (TyFun t1' t2', cs1 ++ cs2)
--     TyBox c ty -> do
--       (c',  cs1) <- fresh c
--       (ty', cs2) <- fresh ty
--       return (TyBox c' ty', cs1 ++ cs2)
--     TyBottom -> return (TyBottom, [])
--     TyLabels ls -> return (TyLabels ls, [])
--     CAdd c1 c2 -> do
--       (c1', cs1) <- fresh c1
--       (c2', cs2) <- fresh c2
--       return (CAdd c1' c2', cs1 ++ cs2)
--     CMul c1 c2 -> do
--       (c1', cs1) <- fresh c1
--       (c2', cs2) <- fresh c2
--       return (CMul c1' c2', cs1 ++ cs2)
--     CSubset c1 c2 -> do
--       (c1', cs1) <- fresh c1
--       (c2', cs2) <- fresh c2
--       return (CSubset c1' c2', cs1 ++ cs2)

-- newtype TypedExp = TypedExp (String, Type, Constraints) deriving (Eq, Ord, Show)
data TypedExp = TypedExp
  { name :: String
  , inferredType :: Type
  , constraints :: Constraints
  , uenv :: UEnv
  }
  deriving (Show)

-- [TODO] 難読。再帰も不明瞭。書き直す
getInterface :: TEnv -> UEnv -> Constraints -> Int -> VL.Module SrcSpanInfo -> ([(TypedExp, Logs)], Int)
getInterface importedTEnv importedUEnv initCon initC (VL.Module _ _ _ decls) =
  let initEnv = Env' initC importedTEnv importedUEnv emptyREnv emptyLogs initLC
      (typedExpLogs, env) = runState (getInterface' $ reverse $ tsortDecls decls) initEnv
      typedExpLogs' = rearrange typedExpLogs
      c = counter env
  in (typedExpLogs', c)
  where
    getInterface' :: [VL.Decl SrcSpanInfo] -> Env [(TypedExp, Logs)]
    getInterface'  [] = return []
    getInterface'  ((VL.PatBind _ pat exp):decls) = do
      decls' <- getInterface' decls

      -- 次の宣言の型検査の前処理
      initializeEnv
      setTEnv importedTEnv
      setUEnv importedUEnv
      forM_ (map fst decls') addDeclToTEnv -- 環境にこれまで生成した束縛・制約を追加

      -- 対象の宣言の型検査
      -- (ty, _, uenv, sub, con) <- infer exp
      (ty, uenv, sub, con) <- infer exp
      let con' = constraintsSubstitution sub con
          -- con' = nub $ map (typeSubstitution sub) con
          -- inc = [ c | c <- con', null $ consOn c `intersect` freeTyVars ty ]
          -- exc = [ c | c <- con', not . null $ consOn c `intersect` freeTyVars ty ]
          exu = filterEnvBy (freeVars ty) uenv
      l <- gets (reverseLogs . log)
      return $
        (TypedExp (getName pat) ty con' exu, l) : decls'

    -- ^ addDeclToTEnv (x : ty, c) => x : [ty]@[a_new] | C U c
    addDeclToTEnv :: TypedExp -> Env ()
    addDeclToTEnv (TypedExp s ty _ _) = do
      tyvar <- genNewTyVar LabelsKind
      putTEnv s (GrType Local ty tyvar)
      -- putCEnv exc

    -- 元の宣言の順番に並び替える
    rearrange :: [(TypedExp, Logs)] -> [(TypedExp, Logs)]
    rearrange = sortOn (\(typedExp, l) -> name typedExp `elemIndex` map getName decls)


-- ^ declsを変数依存関係の下流から上流にtsortする
-- ^ 事前条件:
-- ^ (1) 相互再帰(有向サイクル)を含まない
tsortDecls :: PrettyAST l => [VL.Decl l] -> [VL.Decl l]
tsortDecls decls = tsortBy g getName decls
  where
    nodes = map (\(VL.PatBind _ p _) -> getName p) decls
    edges = distEdges $ map (\(VL.PatBind _ pat exp) -> (getName pat, VL.freeVars exp)) decls
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
    <+> ppE "|" <+> ppE c
    <+> ppE "|" <+> ppE uenv
  ppP (TypedExp s ty c uenv) =
        ppP s <+> colon <+> ppP ty
    <+> ppP "|" <+> ppP c
    <+> ppP "|" <+> ppP uenv

instance PrettyAST [TypedExp] where
  ppE lst = concatWith (surround line) $ map ppE lst
  ppP lst = concatWith (surround line) $ map ppP lst