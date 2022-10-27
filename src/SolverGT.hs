{-# LANGUAGE DataKinds #-}
module SolverGT where

import Prelude

import Data.List (nub)
import qualified Data.Map as M
import Data.Map (Map, fromList, toList, (!), delete, keys, insert, insertWith, size)
import Data.Tuple (swap)
import Data.Maybe (fromMaybe)
import Data.List.Split ( splitOn )

import Control.Monad ( forM_, forM )

import Data.SBV.Trans
import qualified Data.SBV.List as SL

import Syntax.Label
import Syntax.Version
import Syntax.Type
-- import SolverGT hiding (compileLabels)
import Util
import Data.SBV.Trans.Control (CheckSatResult(..), checkSat, getUnsatCore, query)
import Control.Monad.Trans (lift)

-- やること
-- 1. トップレベル宣言が不要になるように書き直す
-- 2. ユニットテストを通るように書き直す(solveからEither値を返す)

-- exmods = [("A", [v100,v101]), ("B", [v100,v101])]
-- idxmods = [("A", 0), ("B", 1)]
-- idxvers = [ ("A", [(v100, 0), (v101, 1)])
--           , ("B", [(v100, 0), (v101, 1)])]
-- idxvers' = [ (0, [(0, v100), (1, v101)])
--            , (1, [(0, v100), (1, v101)])]
-- l = length idxmods

lulist :: (Show a, Show b, Eq a) => [(a, b)] -> a -> b
lulist map k = fromMaybe
              (error $ "cannot find " ++ show k ++ " in map " ++ show map) $
              lookup k map

-- idxVerOf mn v = fromIntegral $ (idxvers `lulist` mn) `lulist` v
-- idxModOf mn = idxmods `lulist` mn

---

type SBit = SInt 2
type SLabel = [[SBit]]
sMayDep, sNotDep, sTBD :: SBit
sMayDep = 1
sNotDep = -1
sTBD = 0

purpose :: String
purpose = "numberOfTBDInVar"

type ErrMsg = String
type VarName = String
type SolverResult = Either (ErrMsg, [String]) (Map VarName Label)

data Env = Env
  { constraints :: Constraints              -- input constraints
  , versionsOfExternalModules :: [(String, [Version])] -- input versions corresponds external modules
  , variables :: [(String, SLabel)] -- List of symbolic variables, corresponding to variable names
  , variablesInConstraints :: [String] -- Type variables in constraints
  , numberOfExternalModules :: Int -- Number of external modules = length of label vectors
  , idxMods :: [(String, Int)] -- List of module names, corresponding to vector indexes
  , idxVers :: [(String, [(Version, Int)])] -- Mapping between versions and indicies in each module
  } deriving (Show)

mkEnv :: Map String [Version] -> Constraints -> Env
mkEnv em cs =
  let mdnames = keys em
      versOfExtMods = toList em -- [TODO] sortする？
      fvCons  = nub $ freeVars cs
      lenMods = length versOfExtMods
      idxMods = zip mdnames [0..]
      idxVers = map (\(mn, vs) -> (mn, zip vs [0..])) versOfExtMods
  in Env
    cs            -- constraints
    versOfExtMods -- versionsOfExternalModules
    []            -- variables
    fvCons        -- variablesInConstraints
    lenMods       -- numberOfExternalModules
    idxMods       -- idxMods
    idxVers       -- idxVers

-- e.g.)
--  em = [("A", [v100,v101]), ("B", [v100,v101])]
--  cs = CSubset (TyVar $ Ident "a0") (TyLabels fromList [("A",[v100,v101])])
solve :: Map String [Version] -> Constraints -> IO SolverResult
solve em cs = do
  let senv = mkEnv em cs
  LexicographicResult res <- optimize Lexicographic $ senario senv
  case res of
    Satisfiable _ _ -> do
      let r :: [(String, Int)] 
          r = toList $ M.map (fromInteger . fromCV) $ delete purpose $ getModelDictionary res
          labels = resToLabels r
      return $ Right labels
    Unsatisfiable _ _ -> return $ Left ("Unsatisfiable", [])
    DeltaSat {}       -> return $ Left ("DeltaSat", [])
    SatExtField _ _   -> return $ Left ("SatExtField", [])
    Unknown _ _       -> return $ Left ("Unknown", [])
    ProofError {}     -> return $ Left ("ProofError",[])

  where
    parseVarName :: String -> (String, String, Version)
    parseVarName str = 
      let [vn,mn,vstr] = splitOn "_" str
          [major,minor,patch] = map (\x -> read x :: Int) $ splitOn "." vstr
      in (vn, mn, Version major minor patch)
    -- [("a0_A_1.0.0", [0,1])] -> fromList [("a0", fromList [("A",[v100,v101])])]
    resToLabels :: [(String, Int)] -> Map String Label
    resToLabels []             = mempty
    resToLabels ((var, bit):ss) =
      let ss' = resToLabels ss in
      if bit == 1
        then
          let (vn, mn, v) = parseVarName var
              oldLabelofVn = fromMaybe mempty $ M.lookup vn ss'
              newLabelOfVn = insertWith (++) mn [v] oldLabelofVn
          in insert vn newLabelOfVn ss'
        else ss'

senario :: Env -> Symbolic ()
senario env = do
  labels <- mapM (mkSLabelSq env) (variablesInConstraints env)
  let svars = map snd labels
      env' = env { variables = labels }
  constrain $ compileConstraints env' (constraints env')
  msMaximize purpose $ numberOfFalse svars
  where
    numberOfTBDInVar :: SLabel -> SInteger
    numberOfTBDInVar = foldr (\row acc -> acc + foldr (\b acc -> acc + ite (b .== sTBD) 1 0) 0 row) 0
    numberOfFalse :: [SLabel] -> SInteger
    numberOfFalse = foldr (\row acc -> acc + numberOfTBDInVar row) 0

mkSLabelSq :: Env -> String -> Symbolic (String, SLabel)
mkSLabelSq env vn = do
  let idxmods = idxMods env
      exmods = versionsOfExternalModules env
  sv <- forM idxmods $ \(mn, mni) -> do
    forM (snd $ exmods !! mni) $ \v -> do
      sInt (putDocString $ ppP vn <> ppP "_" <> ppP mn <> ppP "_" <> ppP v)
  return (vn, sv)

compileConstraints :: Env -> Constraints -> SBool
compileConstraints env cs = case cs of
  CTop -> sTrue
  CSubset coeff1 coeff2 -> do
    let labels = variables env
        v1 = labels `lulist` getName coeff1
    case coeff2 of
      TyLabels ls ->
        subsetOf v1 (compileLabels env ls)
      TyVar v     -> do
        let v2 = labels `lulist` getName coeff2
        subsetOf v1 v2
  CAnd c1 c2 -> 
    let c1' = compileConstraints env c1
        c2' = compileConstraints env c2
    in (.&&) c1' c2'
  COr c1 c2 ->
    let c1' = compileConstraints env c1
        c2' = compileConstraints env c2
    in (.<+>) c1' c2'

-- [("A", [v100,v101]), ("B", [v101])]
-- -> [[sMayDep,sMayDep], [sNotDep,sMayDep]]
compileLabels :: Env -> Label -> SLabel
compileLabels env labels =
  let keysLabels = keys labels
      exmods = versionsOfExternalModules env
  in
    [ row
    | (mn, vers) <- exmods
    , let lvers = length vers
          row = if mn `elem` keysLabels
            then  [ bit
                  | v <- vers 
                  , let bit = if v `elem` (toList labels `lulist` mn) then sMayDep else sNotDep ]
            else replicate lvers sTBD
    ]

subsetOf :: SLabel -> SLabel -> SBool
subsetOf l1 l2 =
  foldl1 (.&&) $ zipWith
    (\row1 row2 ->
      foldl1 (.&&) $ zipWith
        -- 以下のいずれか一方
        -- 1. b2がsMayDep /sNotDep = l2はmnのそのビットの位置のバージョンが候補に含まれる/含まれない
        --    -> l1も同じバージョンに依存する/依存しない
        -- 2. b2がsTBD = l2はmnのそのビットの位置のバージョンについて制約なし
        --    -> l1のについては無制約
        (\b1 b2 ->
          (b2 .== sMayDep .&& b1 .== sMayDep) .<+>
          (b2 .== sNotDep .&& b1 .== sNotDep) .<+>
          (b2 .== sTBD))
        row1 row2)
    l1 l2


test :: Symbolic ()
test = do
  let env = mkEnv em1 cs
      l1 = compileLabels env a100101
      l2 = compileLabels env a100
  constrain $ l1 `subsetOf` l2
  query $ do
    cs <- checkSat
    case cs of
        Unsat -> do
          lift $ print "Unsat"
          return ()
        _     -> do
          lift $ print "Sat"
          return ()

---------------------------

mkVar :: String -> Type
mkVar s = TyVar $ Ident s

mkLabels :: [(String, [Version])] -> Label
mkLabels [] = mempty
mkLabels ((mn,v):rst) =
  let rst' = mkLabels rst
  in Data.Map.insert mn v rst'

mkGtLabels :: String -> Label -> Constraints
mkGtLabels vn labels = CSubset (mkVar vn) (TyLabels labels)

mkGtVar :: String -> String -> Constraints
mkGtVar vn1 vn2 = CSubset (mkVar vn1) (mkVar vn2)

em1 :: Map String [Version]
em1 = fromList
  [ ("A",
    [ Version 1 0 0
    , Version 1 0 1])
  , ("B",
    [ Version 1 0 0
    , Version 1 0 1])
  ]

a100, a101, a100101, b100, b101, b100101 :: Label
a100 = mkLabels [("A", [v100])]
a101 = mkLabels [("A", [v101])]
b100 = mkLabels [("B", [v100])]
b101 = mkLabels [("B", [v101])]
a100101 = mkLabels [("A", [v100, v101])]
b100101 = mkLabels [("B", [v100, v101])]

cand :: [Constraints] -> Constraints
cand = foldr landC CTop

v100, v101 :: Version
v100 = Version 1 0 0
v101 = Version 1 0 1

cs :: Constraints
cs = cand
  [ mkGtLabels "a0" a100
  , mkGtLabels "a0" a101 ]
