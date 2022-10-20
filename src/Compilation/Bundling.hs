module Compilation.Bundling (
  bundle
) where

import Data.Map
import qualified Data.List
import Control.Monad.State
import Control.Monad (forM_)

import DependencyGraph
import Inference.TypeInference hiding (constraints, uenv)
import Syntax.Version (Version)
import Syntax.Type
import Syntax.Env hiding (counter)
import System.Directory (withCurrentDirectory)
import Syntax.Substitution (unify, typeSubstitution)
import qualified Data.Set
import Data.Functor.Classes (Eq1(..))
import Util
import Syntax.Kind (Kind (LabelsKind))
import Syntax.Label (Label(..))

data BundleEnv' = BundleEnv'
  { resources   :: Map String [Version]
  , types       :: Map String [Type]
  , constraints :: Map String Constraints
  , uenv        :: Map String UEnv
  }
  deriving (Show)
type BundleEnv a = State BundleEnv' a

addResource :: String -> Version -> BundleEnv ()
addResource s v = state $ \env ->
  let m' = v : (resources env ! s)
  in ((), env {resources = insert s m' $ resources env})

checkTypes :: [Type] -> Bool
checkTypes [] = True
checkTypes (t:ts) = all (tySym t) ts

genNewLabelsVar :: State Int (Type, UEnv)
genNewLabelsVar = do
  c <- get
  let name = prefixNewTyVar ++ show c
      uenv' = makeEnv [(name, LabelsKind)]
      tyvar = TyVar (Ident name)
  modify $ \c -> c + 1
  return (tyvar, uenv')

-- 各バージョンの同名モジュールのコンパイル結果を受け取り、bundlingされた結果の変数名とリソースのMapを返す
-- 1. そのトップレベルシンボルの全てのバージョンの型がtySymの関係の上で同型か検査
-- 2. そのトップレベルシンボルの実装が存在する全てのバージョンをresourcesに収集
-- 3. そのトップレベルシンボルに関する全てのバージョン制約をconstraintsに収集
bundle :: String -> Int -> Map VLMod [TypedExp] -> (TEnv, UEnv, Constraints, Int)
bundle mn initC m =
  let
    initEnv = BundleEnv' empty empty empty empty
    BundleEnv' vers tys cons mapu = execState collectAcrossVersion initEnv
    ((newTEnv, newUEnv, newCons), newc) = runState
      (foldM
        (\(accTEnv, accUEnv, accCons) sym -> do
          (newLabel, uLabelAdded) <- genNewLabelsVar
          let types = tys ! sym
              TyBox _ ty = head types
          -- types中の型は名前の付け替えを無視すれば全て同一か？
          unless (checkTypes types) $
            error "The value types of all versions must be equal."
          let
            -- U_{x,y,z} (aNew ≤ a_X_x.y.z)
            cs = foldl1 landC $ Data.List.map (\(TyBox r ty) -> CSubset newLabel r) types
            -- aNew ≤ (Available versions)
            newc = CSubset newLabel (TyLabels $ Data.Set.fromList $ Data.List.map (Label <$> Data.Map.singleton mn) $ vers ! sym)
          return
            ( insertEnv sym (GrType Imported ty newLabel) accTEnv -- バンドル後は必ずimported env type
            , accUEnv `Data.Map.union` (mapu ! sym) `Data.Map.union` uLabelAdded
            , accCons `landC` (cons ! sym) `landC` newc `landC` cs
            )
        )
        (emptyEnv :: TEnv, emptyEnv :: UEnv, CTop :: Constraints)
        (keys tys))
      initC
    in
      (newTEnv, newUEnv, newCons, newc)
  where
    collectAcrossVersion :: BundleEnv ()
    collectAcrossVersion =
      let m' = toList m in do
      forM_ m' $ \(VLMod mn v, lstTyExp) -> do
        forM_ lstTyExp $ \(TypedExp s ty c exu) -> do
          -- 型：そのトップレベルシンボルの全てのバージョンの型を収集する
          -- uEnv：全てのバージョンについて収集する
          gets (Data.Map.lookup s . types) >>= \case
            Nothing -> do
              modify ( \env -> env { types = insert s [ty] (types env) } )
              exu' <- gets (insert s exu . uenv)
              modify ( \env -> env { uenv = exu' } )
            Just ty' -> do
              modify ( \env -> env { types = update (\ty' -> Just (ty:ty')) s (types env) } )
              exu' <- gets $ (! s) . uenv
              modify ( \env -> env { uenv = insert s (exu `union` exu') (uenv env) } )
          -- そのトップレベルシンボルの実装が存在する全てのバージョンをresourcesに収集
          gets (Data.Map.lookup s . resources) >>= \case
            Nothing -> do
              res' <- gets (insert s [v] . resources)
              modify ( \env -> env { resources = res' } )
            Just ty -> do
              addResource s v
          -- そのトップレベルシンボルに関する全てのバージョン制約をconstraintsに収集
          gets (Data.Map.lookup s . constraints) >>= \case
            Nothing -> do
              c' <- gets (insert s c . constraints)
              modify ( \env -> env { constraints = c' } )
            Just ty -> do
              c' <- gets (landC c . (! s) .  constraints)
              modify ( \env -> env { constraints = insert s c' (constraints env) } )


