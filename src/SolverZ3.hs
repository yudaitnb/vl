module SolverZ3 where

import Prelude

import Control.Monad ( forM_, forM )
import Control.Monad.Trans (lift)

import Data.List (nub, sort, reverse)
import qualified Data.Map as M
import Data.Map (Map, fromList, toList, delete, keys, insert, insertWith, size, mapWithKey)
import Data.Tuple (swap)
import Data.Maybe (fromMaybe)
import Data.List.Split ( splitOn )

import Data.SBV.Trans
import qualified Data.SBV.List as SL
import Data.SBV.Trans.Control (CheckSatResult(..), checkSat, getUnsatCore, query)

import Syntax.Common hiding (Name(..))

import Syntax.Type
import Compile (DuplicatedExVars(..))
import Parser (VLMod(..))

import Util

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


separator = "---"

lulist :: (Show a, Show b, Eq a) => [(a, b)] -> a -> b
lulist map k = fromMaybe
              (error $ "cannot find " ++ show k ++ " in map " ++ show map) $
              lookup k map

---

type SVersion = SInt8-- バージョン番号を表す、各モジュールのバージョンに対してユニークな値
type SLabel = [SVersion]
sTBD :: SVersion
sTBD = 0
idTBD :: Integer
idTBD = 0

purpose :: String
purpose = "numberOfTBDInVar"

type ErrMsg = String
type SRError = (ErrMsg, [String])
type SRSuccess = Map VarName Label
type SolverResult = Either SRError SRSuccess


combineSolRes :: DuplicatedExVars -> SRSuccess -> Map VarKey (VarKey, VLMod, Type, Label)
combineSolRes evr solres = mapWithKey
  (\vn (s, vlmod, ty) -> (s, vlmod, ty, solres <!> getName ty)) evr

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
      versOfExtMods = toList $ M.map ((TBD :) . reverse . sort) em -- [TODO] 新しいもの順にしたうえで先頭にTBDを追加
      fvCons  = map getVN $ nub $ freeVars cs
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
  -- LexicographicResult res <- optimize Lexicographic $ senario senv
      cfg = defaultSMTCfg {
              transcript = Just "./examples/SMTlib2script.out"
            } -- produce SMT-Lib2 file
  -- LexicographicResult res <- optimizeWith cfg Lexicographic $ senario senv
  -- case res of
  --   Satisfiable _ _ -> do
  --     let r :: [(String, Int)] 
  --         r = toList $ M.map (fromInteger . fromCV) $ delete purpose $ getModelDictionary res
  --     let idxvers = idxVers senv
  --         labels = resToLabels idxvers r
  --     return $ Right labels
  --   Unsatisfiable _ _ -> return $ Left ("Unsatisfiable", [])
  --   DeltaSat {}       -> return $ Left ("DeltaSat", [])
  --   SatExtField _ _   -> return $ Left ("SatExtField", [])
  --   Unknown _ _       -> return $ Left ("Unknown", [])
  --   ProofError {}     -> return $ Left ("ProofError",[])
  SatResult res <- satWith cfg $ senario senv
  case res of
    Satisfiable _ _ -> do
      let r :: [(String, Int)]
          r = toList $ M.map (fromInteger . fromCV) $ delete purpose $ getModelDictionary res
      let idxvers = idxVers senv
          labels = resToLabels idxvers r
      return $ Right labels
    Unsatisfiable _ _ -> return $ Left ("Unsatisfiable", [])
    DeltaSat {}       -> return $ Left ("DeltaSat", [])
    SatExtField _ _   -> return $ Left ("SatExtField", [])
    Unknown _ _       -> return $ Left ("Unknown", [])
    ProofError {}     -> return $ Left ("ProofError",[])

  where
    parseVarName :: String -> (String, String)
    parseVarName str = let [vn,mn] = splitOn separator str in (vn, mn)
    -- [("a0_A_1.0.0", [0,1])] -> fromList [("a0", fromList [("A",[v100,v101])])]
    resToLabels :: [(String, [(Version, Int)])] -> [(String, Int)] -> Map String Label
    resToLabels _       []              = mempty
    resToLabels idxvers ((var, ver):ss) =
      let ss' = resToLabels idxvers ss
          (vn, mn) = parseVarName var
          v = map swap (idxvers `lulist` mn) `lulist` ver
          oldLabelofVn = fromMaybe mempty $ M.lookup vn ss'
          newLabelOfVn = if v == TBD
                          then oldLabelofVn
                          else insertWith (++) mn [v] oldLabelofVn
      in insert vn newLabelOfVn ss'

senario :: Env -> Symbolic ()
senario env = do
  labels <- mapM (mkSLabel env) (variablesInConstraints env)
  let svars = map snd labels
      env' = env { variables = labels }
  allSLabelsAreValid env'
  constrain $ compileConstraints env' (constraints env')
  -- msMinimize purpose $ sTBD

  -- 0,TBD > 1,latest > ...
  -- msMinimize purpose $ sumOfSLabels svars
  where
    sumOfSLabels :: [SLabel] -> SVersion
    sumOfSLabels = foldr (\label acc -> acc + sum label) 0

mkSLabel :: Env -> String -> Symbolic (String, SLabel)
mkSLabel env vn = do
  let idxmods = idxMods env
      exmods = versionsOfExternalModules env
  sv <- forM idxmods $ \(mn, mni) -> do
    sInt8 (putDocString $ ppP vn <> ppP separator <> ppP mn)
  return (vn, sv)

allSLabelsAreValid :: Env -> Symbolic ()
allSLabelsAreValid env = do
  let possibleElems = map (\(mn, vls) -> (mn, map snd vls)) $ idxVers env
  forM_ (map snd $ variables env) $ \sl -> do
    forM_ (zip sl [0..]) $ \(sv, idOfM) -> do
      let mn = map swap (idxMods env) `lulist` idOfM
          possibleElemsOfMn = SL.implode $ map fromIntegral $ possibleElems `lulist` mn
      constrain $ sv `SL.elem` possibleElemsOfMn

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
      _ -> error $
        "\nWhile compiling `cs`, got an unexpected pattern `coeff2`." ++
        "\n    cs: " ++ putDocString (ppP cs) ++
        "\ncoeff2: " ++ putDocString (ppP coeff2)
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
  let idxvers = idxVers env
  in
    [ ver
    | (mn, vers) <- versionsOfExternalModules env
    , let ver = if mn `elem` keys labels
            then
              let versionOfMn = head $ toList labels `lulist` mn -- A -> [v100]型しか存在しないのでheadを取ってきてよい
              in fromIntegral $ idxvers `lulist` mn `lulist` versionOfMn
            else sTBD
    ]

-- a100 <: ?
-- 0 (sTBD) : そのモジュールのバージョンは決まっていない
-- 1~ : そのモジュールはInt8値に対応するバージョン
--     (f x)
--       _ax _afx
-- a_fx        a_x
--  A  B       A   B
-- [v1] <: [v2]
subsetOf :: SLabel -> SLabel -> SBool
subsetOf l1 l2 =
  -- foldl1 (.&&) $ zipWith
  foldl (.&&) sTrue $ zipWith
    -- 以下のいずれか一方
    -- 1. v2が何らかのバージョンを指定している -> v1も同じバージョンに依存する
    -- 2. v2がsTBD, すなわちmnのバージョンについて無制約 -> v1はなんでもよい
    (\v1 v2 ->
           (v2 ./= sTBD .&& v2 .== v1)
      .<+> (v2 .== sTBD))
    l1 l2


-- test :: Symbolic ()
-- test = do
--   let env = mkEnv em1 cs
--       l1 = compileLabels env a100101
--       l2 = compileLabels env a100
--   constrain $ l1 `subsetOf` l2
--   query $ do
--     cs <- checkSat
--     case cs of
--         Unsat -> do
--           lift $ print "Unsat"
--           return ()
--         _     -> do
--           lift $ print "Sat"
--           return ()

---------------------------

-- mkVar :: String -> Type
-- mkVar s = TyVar $ Ident s

-- mkLabels :: [(String, [Version])] -> Label
-- mkLabels [] = mempty
-- mkLabels ((mn,v):rst) =
--   let rst' = mkLabels rst
--   in Data.Map.insert mn v rst'

-- mkGtLabels :: String -> Label -> Constraints
-- mkGtLabels vn labels = CSubset (mkVar vn) (TyLabels labels)

-- mkGtVar :: String -> String -> Constraints
-- mkGtVar vn1 vn2 = CSubset (mkVar vn1) (mkVar vn2)

-- em1 :: Map String [Version]
-- em1 = fromList
--   [ ("A",
--     [ Version 1 0 0
--     , Version 1 0 1])
--   , ("B",
--     [ Version 1 0 0
--     , Version 1 0 1])
--   ]

-- a100, a101, a100101, b100, b101, b100101 :: Label
-- a100 = mkLabels [("A", [v100])]
-- a101 = mkLabels [("A", [v101])]
-- b100 = mkLabels [("B", [v100])]
-- b101 = mkLabels [("B", [v101])]
-- a100101 = mkLabels [("A", [v100, v101])]
-- b100101 = mkLabels [("B", [v100, v101])]

-- cand :: [Constraints] -> Constraints
-- cand = foldr landC CTop

-- v100, v101 :: Version
-- v100 = Version 1 0 0
-- v101 = Version 1 0 1

-- cs :: Constraints
-- cs = cand
--   [ mkGtLabels "a0" a100
--   , mkGtLabels "a0" a101
--   ]


instance PrettyAST (Map VarKey (VarKey, VLMod, Type, Label)) where
  ppE m = concatWith (surround line) $ mapWithKey (\vn (s, m, tv, l) -> ppE vn <+> colon <+> ppE s <> comma <+> ppE m <> comma <+> ppE tv <> comma <+> ppE l) m
  ppP m = concatWith (surround line) $ mapWithKey (\vn (s, m, tv, l) -> ppP vn <+> colon <+> ppP s <> comma <+> ppP m <> comma <+> ppP tv <> comma <+> ppP l) m