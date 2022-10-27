{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances       #-} -- for undetermined s in MonadState
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Solver where

import Control.Monad.Reader
import Control.Monad (forM_, forM)
import Control.Monad.Trans.State.Lazy (StateT)
import Control.Monad.State.Lazy ( MonadState, gets, evalStateT, modify )

import Data.List hiding (isSubsequenceOf)
import Data.Map hiding (elemAt)
import Data.Maybe (fromMaybe)
import Data.Set hiding (elemAt)
import Data.Tuple (swap)

-- import Data.SBV
import Data.SBV.Trans hiding (solve)
import qualified Data.SBV.List as SL
import Data.SBV.Control hiding (Version)
import Data.SBV.Tuple (tuple, (^.), untuple)
import Data.SBV.Set
import Data.SBV.Internals hiding (Label)
import Data.SBV.Tools.BoundedList

import Syntax.Type
import Syntax.Label
import Syntax.Version

import Util hiding (vars)

type SVersion = Integer
type SLabel = (Integer, Integer)

type SolverResult = Either (String, [String]) [(String, [(String, Version)])]

data Env = Env
  { constraints :: Constraints              -- input constraints
  , versionsOfExternalModules :: Map String [Version] -- input versions corresponds external modules
  , variables :: [(String, SList SVersion)] -- List of symbolic variables, corresponding to variable names
  , variablesInConstraints :: [String] -- Type variables in constraints
  , numberOfExternalModules :: Int -- Number of external modules = length of label vectors
  , idxMods :: [(String, Int)] -- List of module names, corresponding to vector indexes
  , idxVers :: Map String (Map Version Int) -- Mapping between versions and indicies in each module
  } deriving (Show)

newtype EnvT m a = EnvT { runEnvT :: StateT Env m a }
  deriving ( Applicative, Functor, Monad, MonadIO, MonadTrans, MonadState Env, MonadFail )

type SolverEnv a = EnvT Symbolic a

runSolverEnv :: (Symbolic a -> t) -> SolverEnv a -> Env -> t
runSolverEnv s (a :: SolverEnv a) (env :: Env) = s $ evalStateT (runEnvT a) env

initEnv :: Env
initEnv = Env CTop Data.Map.empty [] [] 0 [] Data.Map.empty

mkSolverEnv :: Map String [Version] -> Constraints -> Env
mkSolverEnv em cs =
  let mdnames = keys em
      extMods = Data.Map.map (Data.List.reverse . sort) em -- (TBD,) newest, 2nd newest, ...
      fvCons  = nub $ freeVars cs
      lenMods = Data.Map.size extMods
      idxMods = zip mdnames [0..]
      idxVers = Data.Map.map (\vs -> Data.Map.fromList $ (TBD, 0) : zip vs [1..]) extMods
  in Env
    cs      -- constraints
    extMods -- externalModules
    []      -- variables
    fvCons  -- variablesInConstraints
    lenMods -- numberOfExternalModules
    idxMods -- idxMods
    idxVers -- idxVers

------------------------------

solve :: Map String [Version] -> Constraints -> IO SolverResult
solve em cs = do
  let senv = mkSolverEnv em cs
  mbCore <- runSolverEnv runSMT senarioSAT senv
  case mbCore of
    -- Unsatisfiable -> unsat coreから制約を満たさないnamedConstraintのリストを出力
    Just core -> return $ Left ("Unsatisfiable", core)
    -- Satisfiable -> 解集合のうち最般かつ最新のものを出力
    Nothing   -> do
      let optimizeWithStyle = optimize Lexicographic
      LexicographicResult result <- runSolverEnv optimizeWithStyle senarioOPT senv
      case result of
        Satisfiable _ _   -> do
          let res = getModelDictionary result
          let res' = [ (v, l) | v <- variablesInConstraints senv, let l = backToVersion $ fromCV (res `lu` v) ]
          return $ Right res'
        Unsatisfiable _ _ -> error ""
        DeltaSat {}       -> return $ Left ("DeltaSat", [])
        SatExtField _ _   -> return $ Left ("SatExtField", [])
        Unknown _ _       -> return $ Left ("Unknown", [])
        ProofError {}     -> return $ Left ("ProofError",[])
  where
    idxvers' :: Map Int (Map Int (String, Version))
    idxvers' =
      let extMods = Data.Map.map (reverse . sort) em
          idxMods = zip (keys em) [0..] in
      mapKeys (Data.Map.fromList idxMods `lu`) $
        mapWithKey (\k vs -> Data.Map.fromList $
          (0, (k, TBD)) : zip [1..] (zip (repeat k) vs)) extMods
    backToVersion :: [SVersion] -> [(String, Version)]
    backToVersion xs = zipWith (\ i r -> idxvers' `lu` i `lu` r) [0..] (Data.List.map fromIntegral xs)

senarioSAT :: SolverEnv (Maybe [String])
senarioSAT = do
  setOption $ ProduceUnsatCores True
  registerVars =<< gets variablesInConstraints
  vars <- gets variables
  l <- gets numberOfExternalModules
  -- ラベルの各要素は存在するバージョンかTBD
  idxvers <- gets idxVers
  idxmods <- gets idxMods
  forM_ vars $ \(vn, xv) ->
    forM_ idxmods $ \(mn, id) ->
      let vers = Data.Map.keys $ idxvers ! mn
          svers = SL.implode $ Data.List.map fromIntegral $ Data.Map.elems $ idxvers ! mn
          name = "The " ++ show id ++ "th element of " ++ vn ++ " must be in " ++ show vers
      -- ラベルxvのid番目の要素はsversに含まれる
      in namedConstraint name $ (xv SL.!! fromIntegral id) `SL.elem` svers
  -- 全変数の長さはモジュールの数と同じ
  forM_ vars $ \(vn, xv) ->
    -- let name = "variable " ++ show vn ++ " has length " ++ show l
    -- in namedConstraint name $ SL.length xv .== fromIntegral l
    constrain $ SL.length xv .== fromIntegral l
  -- 型推論が生成した制約
  cons <- compileConstraints =<< gets constraints

  lift $ query $ do
    cs <- checkSat
    case cs of
        Unsat -> Just <$> getUnsatCore
        _     -> return Nothing

senarioOPT :: SolverEnv ()
senarioOPT = do
  registerVars =<< gets variablesInConstraints
  vars <- gets variables
  l <- gets numberOfExternalModules
  -- ラベルの各要素は存在するバージョンかTBD
  idxvers <- gets idxVers
  idxmods <- gets idxMods
  forM_ vars $ \(vn, xv) ->
    forM_ idxmods $ \(mn, id) ->
      let vers = Data.Map.keys $ idxvers ! mn
          svers = SL.implode $ Data.List.map fromIntegral $ Data.Map.elems $ idxvers ! mn
          name = "The " ++ show id ++ "th element of " ++ vn ++ " must be in " ++ show vers
      in namedConstraint name $ (xv SL.!! fromIntegral id) `SL.elem` svers
  -- 全変数の長さはモジュールの数と同じ
  forM_ vars $ \(vn, xv) ->
    -- let name = "variable " ++ show vn ++ " has length " ++ show l
    -- in namedConstraint name $ SL.length xv .== fromIntegral l
    constrain $ SL.length xv .== fromIntegral l
  -- 型推論が生成した制約
  cons <- compileConstraints =<< gets constraints

  -- 複数の選択肢のうち最新版
  -- ラベル内の各要素(0->TBD, 1->newest, ..) の総和が最小
  let sumOfVersionNumbers :: [SList SVersion] -> SInteger
      sumOfVersionNumbers = Data.List.foldl (\acc k -> acc + bsum l k) 0
  msMinimize "newest version" $ sumOfVersionNumbers (Data.List.map snd vars)

-----------------------

compileLabels :: Type -> SolverEnv [SBV SLabel]
compileLabels (TyLabels labels) = do
  -- mapM (uncurry mkSLabel) $ Data.Map.toList labels
  let xs =  [ (mn, v)
            | (mn, vers) <- Data.Map.toList labels
            , v <- vers]
  mapM (uncurry mkSLabel) xs
compileLabels _ = error ""

lu :: (Ord a, PrettyAST a, Show a, Show b) => Map a b -> a -> b
varsMap `lu` k = fromMaybe
  (error $ putDocString $ ppP "key :" <+> ppP k <> line <> ppP "map :" <+> ppP (show varsMap) <> line ) $
  Data.Map.lookup k varsMap

compileConstraints :: Constraints -> SolverEnv SBool
compileConstraints cs = do
  vars <- Data.Map.fromList <$> gets variables
  l <- gets numberOfExternalModules
  let isSubsequenceOf' = isSubsequenceOf l
  case cs of
    CSubset c1 c2 -> do
      case c2 of
        TyLabels labels -> do
          let h = getName c1 ++ " <= " ++ putDocString (brackets $ ppP labels)
          ls <- compileLabels c2
          let compiled = foldl1 (.||) [ l `restrict` (vars `lu` getName c1) | l <- ls ]
          -- namedConstraint h compiled
          constrain compiled
          return compiled
        TyVar _ -> do
          let h = getName c1 ++ " <= " ++ getName c2
              compiled = (vars `lu` getName c1) `isSubsequenceOf'` (vars `lu` getName c2)
          -- namedConstraint h compiled
          constrain compiled
          return compiled
        _ -> error ""
    CAnd c1 c2 -> do
      let h = putDocString $ ppP cs
      compiled <- (.&&) <$> compileConstraints c1 <*> compileConstraints c2
      -- namedConstraint h compiled
      constrain compiled
      return compiled
    COr c1 c2 -> do
      let h = putDocString $ ppP cs
      compiled <- (.||) <$> compileConstraints c1 <*> compileConstraints c2
      -- namedConstraint h compiled
      constrain compiled
      return compiled
    CTop -> return sTrue

---------------------

-- restrict : 具象ラベルとリソース変数の比較 (∈)
-- x ≤ ls ([(B:1.0.0), (B:1.0.1)])
-- -> [B:1.0.0] ∈ x || [B:1.0.1] ∈ x
restrict :: SBV SLabel -> SList SVersion -> SBool
restrict s xs =
  let (idxM, idxV) = untuple s
  in xs `SL.elemAt` idxM .== idxV

-- isSubsequentOf : リソース変数同士の比較
-- x ≤ y ->　yのi番目要素が
-- 1. n≠TBDなら、xのi番目の要素もn (yのモジュールxのバージョンがvなら、xのモジュールxのバージョンもv)
-- 2. TBD  なら、xのi番目は何でもよい (yのモジュールxのバージョンがTBDなら、xのモジュールxのバージョンは何でもよい)
-- 制約/集合の上ではyのほうが緩い/大きい(TBDが多い)
isSubsequenceOf :: Int -> SList SVersion -> SList SVersion -> SBool
isSubsequenceOf l s1 s2 = band l $ bzipWith l f s1 s2
  where
    f :: SBV SVersion -> SBV SVersion -> SBool
    f x1 x2 = ite (x2 .== 0) sTrue (x1 .== x2) -- x2がTBDの時以外はバージョンが一致することが部分列の条件

---------------------

mkVars :: [String] -> SolverEnv (Map String (SList SVersion))
mkVars [] = return Data.Map.empty
mkVars (s:fv) = do
  x :: SList SVersion <- sList s
  Data.Map.insert s x <$> mkVars fv

registerVars :: [String] -> SolverEnv ()
registerVars ss = do
  forM_ ss $ \s -> do
    x :: SList SVersion <- sList s
    modify $ \env -> env { variables = (s, x) : variables env }

mkVer :: Int -> Int -> Int -> Version
mkVer = Version

mkVar :: String -> Type
mkVar s = TyVar $ Ident s

mkLabels :: [(String, [Version])] -> Label
mkLabels [] = mempty
mkLabels ((mn,vers):rst) =
  let rst' = mkLabels rst
  in Data.Map.insert mn vers rst'

mkSLabel :: String -> Version -> SolverEnv (SBV SLabel)
mkSLabel s v = do
  l <- gets numberOfExternalModules
  idxmods <- gets $ Data.Map.fromList . idxMods
  idxvers <- gets idxVers
  return $ tuple (fromIntegral (idxmods `lu` s), fromIntegral (idxvers `lu` s `lu` v))

-- testCons :: Constraints
-- testCons =
--   [ CSubset (mkVar "a51") (mkTyLabels [b100, b101])
--   , CSubset (mkVar "a51") (mkVar "a0")
--   , CSubset (mkVar "a51") (mkVar "a24")

--   , CSubset (mkVar "a48") (mkTyLabels [b100, b101])
--   , CSubset (mkVar "a48") (mkVar "a11")
--   , CSubset (mkVar "a48") (mkVar "a35")

--   , CSubset (mkVar "a56") (mkVar "a51")
--   , CSubset (mkVar "a56") (mkVar "a48")

--   , CSubset (mkVar "a59") (mkVar "a51")
--   , CSubset (mkVar "a59") (mkVar "a48")

--   , CSubset (mkVar "a60") (mkTyLabels [a100, a101])
--   , CSubset (mkVar "a60") (mkVar "a56")
--   , CSubset (mkVar "a60") (mkVar "a59")

--   , CSubset (mkVar "a39") (mkVar "a60")

--   , CSubset (mkVar "a49") (mkTyLabels [b100])
--   , CSubset (mkVar "a49") (mkVar "a47")

--   , CSubset (mkVar "a61") (mkVar "a49")
--   , CSubset (mkVar "a61") (mkVar "a60")

--   , CSubset (mkVar "a15") (mkVar "a60")

--   , CSubset (mkVar "a50") (mkTyLabels [b101])
--   , CSubset (mkVar "a50") (mkVar "a23")

--   , CSubset (mkVar "a62") (mkVar "a50")
--   , CSubset (mkVar "a62") (mkVar "a60")

--   , CSubset (mkVar "a71") (mkVar "a50")
--   , CSubset (mkVar "a71") (mkVar "a49")
--   , CSubset (mkVar "a71") (mkVar "a60")

--   , CSubset (mkVar "a3") (mkVar "a51")
--   , CSubset (mkVar "a3") (mkVar "a48")
--   , CSubset (mkVar "a3") (mkVar "a51")

--   , CSubset (mkVar "a5") (mkVar "a3")

--   , CSubset (mkVar "a29") (mkVar "a27")
--   , CSubset (mkVar "a41") (mkVar "a39")
--   , CSubset (mkVar "a17") (mkVar "a15")

--   ]
--   where
--     mkTyLabels :: [Label] -> Type
--     mkTyLabels ls = TyLabels $ Data.Set.fromList ls
--     a100 = mkLabel [("A", v100)]
--     a101 = mkLabel [("A", v101)]
--     b100 = mkLabel [("B", v100)]
--     b101 = mkLabel [("B", v101)]
--     v100 = mkVer 1 0 0
--     v101 = mkVer 1 0 1

--------------------------------------------

instance MonadSymbolic m => MonadSymbolic (EnvT m) where
instance (SolverContext m, Monad m) => SolverContext (EnvT m) where
  constrain :: (SolverContext m, Monad m) => SBool -> EnvT m ()
  constrain sbool = lift $ constrain sbool
  softConstrain :: (SolverContext m, Monad m) => SBool -> EnvT m ()
  softConstrain sbool = lift $ softConstrain sbool
  namedConstraint :: (SolverContext m, Monad m) => String -> SBool -> EnvT m ()
  namedConstraint s sbool = lift $ namedConstraint s sbool
  constrainWithAttribute :: (SolverContext m, Monad m) => [(String, String)] -> SBool -> EnvT m ()
  constrainWithAttribute attr sbool = lift $ constrainWithAttribute attr sbool
  setOption :: (SolverContext m, Monad m) => SMTOption -> EnvT m ()
  setOption op = lift $ setOption op
  addAxiom :: (SolverContext m, Monad m) => String -> [String] -> EnvT m ()
  addAxiom s ss = lift $ addAxiom s ss
  contextState :: (SolverContext m, Monad m) => EnvT m Data.SBV.Internals.State
  contextState = lift contextState
  addSMTDefinition :: (SolverContext m, Monad m) => String -> [String] -> EnvT m ()
  addSMTDefinition s ss = lift $ addAxiom s ss
