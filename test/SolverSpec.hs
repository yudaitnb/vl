module SolverSpec (spec) where

import Test.Hspec
import Prelude
import qualified Data.Set hiding (map)

import Syntax.Label
import Syntax.Version
import Syntax.Type
import Solver
import qualified Data.Map
import qualified Data.Either


-- test :: IO ()
-- test = do
--   let fvCons = freeVars testConstraints
--       maxWHeader = maximum $ map length fvCons
--       maxWItem = 7 -- [TODO]
--       printResult :: Int -> Int -> [(String, [(String, Version)])] -> IO ()
--       printResult maxWHeader maxWItem m = do
--         forM_ m $ \(v, res) -> do
--           let header = fill maxWHeader (ppP v) <+> ppP ":"
--               maintxt = concatWith (surround $ comma <> space) $ map (\(mn, v) -> fill maxWItem $ ppP mn <> colon <> ppP v) res
--               doc = header <+> maintxt
--           print $ putDocString doc
--   solve testExtMods testConstraints >>= \case
--     Left (h,r)   -> do
--         print h
--         print $ putDocString $ concatWith (surround $ comma <> space) $ map ppP r
--     Right res -> printResult maxWHeader maxWItem res

spec :: Spec
spec = do
  describe "Satisfiable" $ do
    it "a0 <= a100" $ do
      let cs = cand [ mkGtLabels "a0" a100 ]
      res <- Data.Either.fromRight (error "") <$> solve em1 cs
      res `shouldBe` [("a0", [("A", v100), ("B", tbd)])]
    it "a0 <= a100101" $ do
      let cs = cand [ mkGtLabels "a0" a100101 ]
      res <- Data.Either.fromRight (error "") <$> solve em1 cs
      res `shouldBe` [("a0", [("A", v101), ("B", tbd)])]
    it "a0 <= a100, a1 <= a100" $ do
      let cs = cand [ mkGtLabels "a0" a100, mkGtLabels "a1" a100 ]
      res <- Data.Either.fromRight (error "") <$> solve em1 cs
      res `shouldBe`
        [ ("a0", [("A", v100), ("B", tbd)])
        , ("a1", [("A", v100), ("B", tbd)]) ]
    it "a0 <= a1, a1 <= a100" $ do
      let cs = cand [ mkGtVar "a0" "a1", mkGtLabels "a1" a100 ]
      res <- Data.Either.fromRight (error "") <$> solve em1 cs
      res `shouldBe`
        [ ("a0", [("A", v100), ("B", tbd)])
        , ("a1", [("A", v100), ("B", tbd)]) ]
    it "a0 <= a1, a1 <= a100101, a2 <= a100101" $ do
      let cs = cand [ mkGtVar "a0" "a1", mkGtLabels "a1" a100101, mkGtLabels "a2" a100101 ]
      res <- Data.Either.fromRight (error "") <$> solve em1 cs
      res `shouldBe`
        [ ("a0", [("A", v101), ("B", tbd)])
        , ("a1", [("A", v101), ("B", tbd)])
        , ("a2", [("A", v101), ("B", tbd)])]
    it "a0 <= a1, a1 <= a100, a2 <= a100101" $ do
      let cs = cand [ mkGtVar "a0" "a1", mkGtLabels "a1" a100, mkGtLabels "a2" a100101 ]
      res <- Data.Either.fromRight (error "") <$> solve em1 cs
      res `shouldBe`
        [ ("a0", [("A", v100), ("B", tbd)])
        , ("a1", [("A", v100), ("B", tbd)])
        , ("a2", [("A", v101), ("B", tbd)])]
  describe "Unsatisfiable" $ do
    it "a0 <= a100, a0 <= a101" $ do
      let cs = cand [ mkGtLabels "a0" a100, mkGtLabels "a0" a101 ]
      res <- Data.Either.isLeft <$> solve em1 cs
      res `shouldBe` True
    it "a0 <= a1, a1 <= a100, a0 <= a2, a2 <= a101" $ do
      let cs = cand [ mkGtVar "a0" "a1", mkGtLabels "a1" a100, mkGtVar "a0" "a2", mkGtLabels "a2" a101 ]
      res <- Data.Either.isLeft <$> solve em1 cs
      res `shouldBe` True
  describe "Practical example (Satisfiable)" $ do
    it "a0 <= [(B:1.0.0)], a1 <= [(B:1.0.1)], a2 <= [(B:1.0.0),(B:1.0.1)], a3 <= a0, a3 <= a2, a4 <= a1, a4 <= a2" $ do
      let cs = cand 
               [ mkGtLabels "a0" b100
               , mkGtLabels "a1" b101
               , mkGtLabels "a2" b100101
               , mkGtVar "a3" "a0"
               , mkGtVar "a3" "a2"
               , mkGtVar "a4" "a1"
               , mkGtVar "a4" "a2"
               ]
      res <- Data.Either.fromRight (error "") <$> solve em1 cs
      Data.Set.fromList res `shouldBe` Data.Set.fromList
        [ ("a0",  [("A", tbd), ("B", v100)])
        , ("a1",  [("A", tbd), ("B", v101)])
        , ("a2",  [("A", tbd), ("B", v101)])
        , ("a3",  [("A", tbd), ("B", v100)])
        , ("a4",  [("A", tbd), ("B", v101)])
        ]
    it "a67 <= [(B:1.0.0)], a79 <= a68, a68 <= [(B:1.0.0), (B:1.0.1)], a103 <= a80, a103 <= a65, a65 <= [(B:1.0.0)], a97 <= a80, a97 <= a66, a66 <= [(B:1.0.1)], a80 <= a79" $ do
      let cs = cand 
            [ mkGtLabels "a67" b100
            , mkGtVar "a79" "a68"
            , mkGtLabels "a68" b100101
          --  , mkGtVar "a103" "a80"
          --  , mkGtVar "a103" "a65"
            , mkGtLabels "a65" b100
            , mkGtVar "a97" "a80"
            , mkGtVar "a97" "a66"
            , mkGtLabels "a66" b101
            , mkGtVar "a80" "a79"
            ]
      res <- Data.Either.fromRight (error "") <$> solve em1 cs
      Data.Set.fromList res `shouldBe` Data.Set.fromList
        [ ("a67",  [("A", tbd), ("B", v100)])
        , ("a79",  [("A", tbd), ("B", v101)])
        , ("a68",  [("A", tbd), ("B", v101)])
        -- , ("a103", [("A", tbd), ("B", v100)])
        , ("a80",  [("A", tbd), ("B", v101)])
        , ("a65",  [("A", tbd), ("B", v100)])
        , ("a97",  [("A", tbd), ("B", v101)])
        , ("a66",  [("A", tbd), ("B", v101)])
        ]

-- a:[Int]_(a80),
-- f:[(Int@[a3] -> Int)]_(a64),
-- g:[(Int@[a39] -> Int)]_(a65),
-- h:[(Int@[a15] -> Int)]_(a66),
-- w:[(Int@[a52] -> (Int@[a55] -> Int))]_(a67),
-- y:[Int]_(a68)
-- [(f [y])] : Int@[a79]
-- [(h [a])] : Int@[a97]
-- [(g [a])] : Int@[a103]
-- mkGtLabels "a67" b100
-- mkGtVar "a79" "a68"
-- mkGtLabels "a68" [b100, b101]
-- mkGtVar "a103" "a80"
-- mkGtVar "a103" "a65"
-- mkGtLabels "a65" b100
-- mkGtVar "a97" "a80"
-- mkGtVar "a97" "a66"
-- mkGtLabels "a66" b101
-- mkGtVar "a80" "a79"

-------------------

mkGtLabels :: String -> Labels -> Constraints
mkGtLabels vn labels = CSubset (mkVar vn) (TyLabels labels)

mkGtVar :: String -> String -> Constraints
mkGtVar vn1 vn2 = CSubset (mkVar vn1) (mkVar vn2)

em1 :: Data.Map.Map String [Version]
em1 = Data.Map.fromList
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
cand [] = CTop
cand (c:cs) = CAnd c (cand cs)

v100, v101, tbd :: Version
v100 = mkVer 1 0 0
v101 = mkVer 1 0 1
tbd  = TBD