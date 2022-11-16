module Translation.Bundling (
  bundle
) where

import Control.Monad.State
import Control.Monad (forM_)

import Data.Map
import qualified Data.List

import Language.Haskell.Exts (KnownExtension(ConstraintKinds))

import Parser
import Inference.TypeInference hiding (constraints, uenv)

import Syntax.Common (Version, ModName(..), VarName(..))
import Syntax.Type
import Syntax.Env hiding (counter)
import Syntax.Kind ( Kind(LabelsKind) )

import Util


data CollectEnv' = CollectEnv'
  { resources   :: Map ModName [Version]
  , types       :: Map ModName (Map Version Type)
  , constraints :: Map ModName (Map Version Constraints)
  , uenv        :: Map ModName UEnv
  }
  deriving (Show)
type CollectEnv a = State CollectEnv' a

addResource :: ModName -> Version -> CollectEnv ()
addResource mn v = modify $ \env ->
  let m' = v : (resources env <!> mn)
  in env {resources = insert mn m' $ resources env}

checkTypes :: Map Version Type -> Bool
checkTypes tys = 
  let t = snd . head $ toList tys
  in all (tySym t) (elems tys)

data BundleEnv' = BundleEnv'
  { counter      :: Int
  , bTEnv        :: TEnv
  , bUEnv        :: UEnv
  , bConstraints :: Constraints
  , bConsScheme  :: Map VarName Constraints 
  }
  deriving (Show)
type BundleEnv a = State BundleEnv' a

-- 新しいラベル変数を生成し、型変数環境をその新ラベル変数で更新
genNewLabelsVar :: BundleEnv Type
genNewLabelsVar = do
  c <- gets counter
  let name = prefixNewTyVar ++ show c
      tyvar = TyVar (Ident name)
  modify $ \env -> env
    { counter = 1 + counter env
    , bUEnv = insert name LabelsKind (bUEnv env) }
  return tyvar

-- 見本の型を一つ受け取り、その型と同型でリソース変数だけ新しい型を生成する
-- f : !_a1 (!_a0 Int -> Int) -> f : f : !_a2 (!_a3 Int -> Int) (a2,a3は逆でも問題なし)
genSampleType :: Type -> BundleEnv Type
genSampleType ty = case ty of
  TyCon _     -> return ty
  TyVar name  -> genNewLabelsVar
  TyFun t1 t2 -> TyFun <$> genSampleType t1 <*> genSampleType t2
  TyBox c t   -> TyBox <$> genSampleType c <*> genSampleType t
  TyBottom    -> return ty
  TyLabels _  -> return ty
  TyTuple ts  -> TyTuple <$> mapM genSampleType ts
  TyList ty   -> TyList <$> genSampleType ty

-- 特定のバージョンの型と見本の型を受け取り、その間の制約を生成する
genConstraints :: Type -> Type -> BundleEnv Constraints
genConstraints t1 t2 = case (t1, t2) of
  (TyCon _, TyCon _) -> return CTop
  (TyVar _, TyVar _) -> return $ CSubset t1 t2
  (TyFun t1 t2, TyFun t1' t2') -> landC <$> genConstraints t1 t1' <*> genConstraints t2 t2'
  (TyBox c t, TyBox c' t') -> landC <$> genConstraints c c' <*> genConstraints t t'
  (TyTuple ts, TyTuple ts') -> do
    cs <- zipWithM genConstraints ts ts'
    foldM (\c1 c2 -> return $ c1 `landC` c2) emptyConstraints cs
  (TyList t, TyList t') -> genConstraints t t'
  (TyBottom, TyBottom)   -> return CTop
  (TyLabels _, TyLabels _) -> return CTop

-- 各バージョンの同名モジュールのコンパイル結果を受け取り、bundlingされた結果の変数名とリソースのMapを返す
-- 1. そのトップレベルシンボルの全てのバージョンの型がtySymの関係の上で同型か検査
-- 2. そのトップレベルシンボルの実装が存在する全てのバージョンをresourcesに収集
-- 3. そのトップレベルシンボルに関する全てのバージョン制約をconstraintsに収集
bundle :: ModName -> Int -> Map VLMod [TypedExp] -> (TEnv, UEnv, Constraints, Map VarName Constraints, Int)
bundle mn initC m =
  let
    CollectEnv' vers tys cons mapu = execState collectAcrossVersion (CollectEnv' empty empty empty empty)
    BundleEnv' newCounter newTEnv newUEnv newCons newScheme =
      execState
        (forM_ (toList tys) $ \(vn, tysForV) -> bundleOfVnMn vn mn tysForV)
        (BundleEnv' initC mempty mempty CTop mempty)
  in  ( newTEnv
      , newUEnv
      , newCons
      , newScheme
      , newCounter)
  where
    -- あるシンボルのバージョン毎の型を受け取り、bundle後のTEnv, 増えたラベルのUEnv, 増えたラベルと前のラベルの間のリソースを計算する
    bundleOfVnMn :: VarName -> ModName -> Map Version Type -> BundleEnv ()
    bundleOfVnMn vn mn tymap = do
      let tymaplst = toList tymap
          tyhead = snd . head $ tymaplst
      sampleTy <- genSampleType tyhead
      let TyBox samc samty = sampleTy
      let newEnvTy = GrType Imported samty samc
      csOfV <- fmap (foldl1 lorC) $ forM tymaplst $ \(v, ty) -> do
        landC
          <$> genConstraints sampleTy ty
          <*> return (CSubset samc (TyLabels (singleton mn [v])))
      modify $ \env -> env
        { bConstraints = csOfV `landC` bConstraints env
        , bTEnv = insert vn newEnvTy (bTEnv env)
        , bConsScheme = insert vn csOfV (bConsScheme env) } -- bUEnvはラベル生成時に更新済み
  
    collectAcrossVersion :: CollectEnv ()
    collectAcrossVersion =
      let m' = toList m in do
      forM_ m' $ \(VLMod mn v, lstTyExp) -> do
        forM_ lstTyExp $ \(TypedExp s ty c exu) -> do
          -- 型：そのトップレベルシンボルの全てのバージョンの型を収集する
          -- uEnv：全てのバージョンについて収集する
          gets (Data.Map.lookup s . types) >>= \case
            Nothing -> do
              modify ( \env -> env { types = insert s (singleton v ty) (types env) } )
              exu' <- gets (insert s exu . uenv)
              modify ( \env -> env { uenv = exu' } )
            Just ty' -> do
              oldTysOfS <- gets $ (<!> s) . types
              let newTysOfS = insert v ty oldTysOfS
              modify ( \env -> env { types = insert s newTysOfS (types env) } )
              exu' <- gets $ (<!> s) . uenv
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
              c' <- gets (insert s (singleton v c) . constraints)
              modify ( \env -> env { constraints = c' } )
            Just ty -> do
              oldCsOfS <- gets $ (<!> s) .constraints
              let newCsOfS = insert v c oldCsOfS 
              -- c' <- gets (landC c . (<!> s) .  constraints)
              modify ( \env -> env { constraints = insert s newCsOfS (constraints env) } )


