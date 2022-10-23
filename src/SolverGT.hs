module SolverGT where

import Prelude

import qualified Data.Map as M
import Data.Map (Map, fromList, toList, (!), delete, keys, insert, insertWith)
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

-- やること
-- 1. トップレベル宣言が不要になるように書き直す
-- 2. ユニットテストを通るように書き直す(solveからEither値を返す)

vers = [[0,1],[0,1]]
exmods = [("A", [v100,v101]), ("B", [v100,v101])]
idxmods = [("A", 0), ("B", 1)]
idxvers = [ ("A", [(v100, 0), (v101, 1)])
          , ("B", [(v100, 0), (v101, 1)])]
idxvers' = [ (0, [(0, v100), (1, v101)])
           , (1, [(0, v100), (1, v101)])]
l = length idxmods

lulist :: (Show a, Show b, Eq a) => [(a, b)] -> a -> b
lulist map k = fromMaybe
              (error $ "cannot find" ++ show k ++ "in map " ++ show map) $
              lookup k map

idxVerOf mn v = fromIntegral $ (idxvers `lulist` mn) `lulist` v
idxModOf mn = idxmods `lulist` mn
purpose = "Mazimize false"

testIO :: IO ()
testIO = do
  LexicographicResult res <- optimize Lexicographic test
  case res of
    Satisfiable _ _ -> do
      let r = toList $ M.map fromCV $ delete purpose $ getModelDictionary res
          pp = resToLabels r
      forM_ (toList pp) $ \(vn, l) -> do
        putDoc $ ppP vn <+> ppP "->" <+> ppP l <> line
  where
    parseVarName :: String -> (String, String, Version)
    parseVarName str = 
      let [vn,mn,vstr] = splitOn "_" str
          [major,minor,patch] = map (\x -> read x :: Int) $ splitOn "." vstr
      in (vn, mn, Version major minor patch)
    -- [("a0_A_1.0.0", [0,1])] -> fromList [("a0", fromList [("A",[v100,v101])])]
    resToLabels :: [(String, Bool)] -> Map String Labels
    resToLabels []             = mempty
    resToLabels ((var, bit):ss) =
      let ss' = resToLabels ss in
      if bit
        then let (vn, mn, v) = parseVarName var
                 oldLabelofVn = fromMaybe mempty $ M.lookup vn ss'
                 newLabelOfVn = insertWith (++) mn [v] oldLabelofVn
             in insert vn newLabelOfVn ss'
        else ss'

test :: Symbolic ()
test = do
  let vars = ["a0", "a1"]
  labels <- mapM mkSLabelSq vars
  let svars = map snd labels
  (labels `lulist` "a0") `subsetOf` compileLabels a100101
  (labels `lulist` "a1") `subsetOf` (labels `lulist` "a0")
  msMinimize purpose $ numberOfFalse svars
  where
    numberOfFalseInVar :: [[SBool]] -> SInteger
    numberOfFalseInVar = foldr (\row acc -> acc + foldr (\b acc -> acc + ite b 1 0) 0 row) 0
    numberOfFalse :: [[[SBool]]] -> SInteger
    numberOfFalse = foldr (\row acc -> acc + numberOfFalseInVar row) 0

-- [("A", [v100,v101]), ("B", [v101])]
-- -> [[True,True], [False,True]]
compileLabels :: Labels -> [[SBool]]
compileLabels labels =
  let keysLabels = keys labels in
  [ bits
  | (mn, vers) <- exmods
  , let lvers = length vers
        bits = if mn `elem` keysLabels
                  then  [ bit
                        | v <- vers 
                        , let bit = if v `elem` (toList labels `lulist` mn)
                                      then sTrue
                                      else sFalse ]
                  else replicate lvers sFalse
  ]

mkSLabelSq :: String -> Symbolic (String, [[SBool]])
mkSLabelSq vn = do
  sv <- forM idxmods $ \(mn, mni) -> do
    forM (snd $ exmods !! mni) $ \v -> do
      sBool (putDocString $ ppP vn <> ppP "_" <> ppP mn <> ppP "_" <> ppP v)
  return (vn, sv)

subsetOf :: [[SBool]] -> [[SBool]] -> Symbolic ()
subsetOf l1 l2 = do
  forM_ (zip l1 l2) $ \(r1, r2) -> do
    forM_ (zip r1 r2) $ \(b1, b2) -> do
      constrain $
        -- 以下のいずれか一方
        -- 1. b2がtrue = l2はmnのそのビットの位置のバージョンへの依存性を持つ
        --    -> l1も同じバージョンに依存
        -- 2. b2がfalse = l2はmnのそのビットの位置のバージョンに依存性を持つとは限らない
        --    -> l1のについては無制約
        (b2 .== sTrue .&& b1 .== b2) .|| (b2 .== sFalse) 

---------------------------

mkVar :: String -> Type
mkVar s = TyVar $ Ident s

mkLabels :: [(String, [Version])] -> Labels
mkLabels [] = mempty
mkLabels ((mn,v):rst) =
  let rst' = mkLabels rst
  in Data.Map.insert mn v rst'

mkGtLabels :: String -> Labels -> Constraints
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

a100, a101, a100101, b100, b101, b100101 :: Labels
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
cs = cand [ mkGtLabels "a0" a100 ]