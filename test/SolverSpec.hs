module SolverSpec (spec) where

import Test.Hspec
import Prelude

import Syntax.Common hiding (Name(..), QName(..))
import Syntax.Type
import Solver
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Either
import qualified Data.Bifunctor


spec :: Spec
spec = do
  describe "mkOrdLstOfAndCs" $ do
    it "a0 <= a100" $ do
      let cs = cand [ mkGtLabels "a0" a100 ]
          res = mkOrdLstOfAndCs cs
      res `shouldBe` [ mkGtLabels "a0" a100 ]
    it "a0 <= a1, a1 <= a100" $ do
      let cs = cand [ mkGtVar "a0" "a1", mkGtLabels "a1" a100 ]
          res = mkOrdLstOfAndCs cs
      res `shouldBe` [ mkGtLabels "a1" a100, mkGtVar "a0" "a1" ]
  describe "Satisfiable" $ do
    it "a0 <= a100" $ do
      let cs = cand [ mkGtLabels "a0" a100 ]
      res <- Data.Either.fromRight (error "") <$> solve em1 cs
      res `shouldBe` mkLabels [("a0", [("A", [v100])])]
    it "a0 <= a100, a1 <= a100" $ do
      let cs = cand [ mkGtLabels "a0" a100, mkGtLabels "a1" a100 ]
      res <- Data.Either.fromRight (error "") <$> solve em1 cs
      res `shouldBe` mkLabels 
        [ ("a0", [("A", [v100])])
        , ("a1", [("A", [v100])]) ]
    it "a0 <= a1, a1 <= a100" $ do
      let cs = cand [ mkGtVar "a0" "a1", mkGtLabels "a1" a100 ]
      res <- Data.Either.fromRight (error "") <$> solve em1 cs
      res `shouldBe` mkLabels 
        [ ("a0", [("A", [v100])])
        , ("a1", [("A", [v100])]) ]
    it "a0 <= a100 or a0 <= 101" $ do
      let cs = cand [ mkGtLabels "a0" a100 `COr` mkGtLabels "a0" a101 ]
      res <- Data.Either.fromRight (error "") <$> solve em1 cs
      res `shouldBe` mkLabels [("a0", [("A", [v101])])]
    it "a0 <= a1, a1 <= a100101, a2 <= a100101" $ do
      let cs = cand [
              mkGtVar "a0" "a1"
            , mkGtLabels "a1" a100 `COr` mkGtLabels "a1" a101
            , mkGtLabels "a2" a100 `COr` mkGtLabels "a2" a101
            ]
      res <- Data.Either.fromRight (error "") <$> solve em1 cs
      res `shouldBe` mkLabels 
        [ ("a0", [("A", [v101])])
        , ("a1", [("A", [v101])])
        , ("a2", [("A", [v101])])]
    it "a0 <= a1, a1 <= a100, a2 <= a100101" $ do
      let cs = cand [
              mkGtVar "a0" "a1"
            , mkGtLabels "a1" a100
            , mkGtLabels "a2" a100 `COr` mkGtLabels "a2" a101
            ]
      res <- Data.Either.fromRight (error "") <$> solve em1 cs
      res `shouldBe` mkLabels 
        [ ("a0", [("A", [v100])])
        , ("a1", [("A", [v100])])
        , ("a2", [("A", [v101])])]
  describe "Unsatisfiable" $ do
    it "a0 <= a100, a0 <= a101" $ do
      let cs = cand [ mkGtLabels "a0" a100, mkGtLabels "a0" a101 ]
      res <- Data.Either.isLeft <$> solve em1 cs
      res `shouldBe` True
    it "a0 <= a1, a1 <= a100, a0 <= a2, a2 <= a101" $ do
      let cs = cand [ mkGtVar "a0" "a1", mkGtLabels "a1" a100, mkGtVar "a0" "a2", mkGtLabels "a2" a101 ]
      res <- Data.Either.isLeft <$> solve em1 cs
      res `shouldBe` True
  -- describe "Practical example (Unsatisfiable)" $ do
  --   it "a0 <= [(B:1.0.0)], a1 <= [(B:1.0.1)], a2 <= [(B:1.0.0),(B:1.0.1)], a3 <= a0, a3 <= a2, a4 <= a1, a4 <= a2" $ do
  --     let cs = cand 
  --              [ mkGtLabels "a0" b100
  --              , mkGtLabels "a1" b101
  --              , mkGtLabels "a2" b100101
  --              , mkGtVar "a3" "a0"
  --              , mkGtVar "a3" "a2"
  --              , mkGtVar "a4" "a1"
  --              , mkGtVar "a4" "a2"
  --              ]
  --     res <- Data.Either.fromRight (error "") <$> solve em1 cs
  --     Data.Set.fromList res `shouldBe` Data.Set.fromList
  --       [ ("a0",  [("A", tbd), ("B", [v100])])
  --       , ("a1",  [("A", tbd), ("B", [v101])])
  --       , ("a2",  [("A", tbd), ("B", [v101])])
  --       , ("a3",  [("A", tbd), ("B", [v100])])
  --       , ("a4",  [("A", tbd), ("B", [v101])])
  --       ]
  --   it "a67 <= [(B:1.0.0)], a79 <= a68, a68 <= [(B:1.0.0), (B:1.0.1)], a103 <= a80, a103 <= a65, a65 <= [(B:1.0.0)], a97 <= a80, a97 <= a66, a66 <= [(B:1.0.1)], a80 <= a79" $ do
  --     let cs = cand 
  --           [ mkGtLabels "a67" b100
  --           , mkGtVar "a79" "a68"
  --           , mkGtLabels "a68" b100101
  --         --  , mkGtVar "a103" "a80"
  --         --  , mkGtVar "a103" "a65"
  --           , mkGtLabels "a65" b100
  --           , mkGtVar "a97" "a80"
  --           , mkGtVar "a97" "a66"
  --           , mkGtLabels "a66" b101
  --           , mkGtVar "a80" "a79"
  --           ]
  --     res <- Data.Either.fromRight (error "") <$> solve em1 cs
  --     Data.Set.fromList res `shouldBe` Data.Set.fromList
  --       [ ("a67",  [("A", tbd), ("B", [v100])])
  --       , ("a79",  [("A", tbd), ("B", [v101])])
  --       , ("a68",  [("A", tbd), ("B", [v101])])
  --       -- , ("a103", [("A", tbd), ("B", [v100])])
  --       , ("a80",  [("A", tbd), ("B", [v101])])
  --       , ("a65",  [("A", tbd), ("B", [v100])])
  --       , ("a97",  [("A", tbd), ("B", [v101])])
  --       , ("a66",  [("A", tbd), ("B", [v101])])
  --       ]

-------------------

mkGtLabels :: String -> Label -> Constraints
mkGtLabels vn labels = CSubset (mkVar vn) (TyLabels labels)

mkGtVar :: String -> String -> Constraints
mkGtVar vn1 vn2 = CSubset (mkVar vn1) (mkVar vn2)

em1 :: Map String [Version]
em1 = M.fromList
  [ ("A",
    [ Version 1 0 0
    , Version 1 0 1])
  , ("B",
    [ Version 1 0 0
    , Version 1 0 1])
  ]

a100, a101, b100, b101 :: Label
a100 = mkLabel [("A", [v100])]
a101 = mkLabel [("A", [v101])]
b100 = mkLabel [("B", [v100])]
b101 = mkLabel [("B", [v101])]

cand :: [Constraints] -> Constraints
cand = foldr CAnd CTop

v100, v101, tbd :: Version
v100 = Version 1 0 0
v101 = Version 1 0 1
tbd  = TBD

mkLabel :: [(ModName, [Version])] -> Label
mkLabel  = M.fromList

mkLabels :: [(VarName, [(ModName, [Version])])] -> Map VarName Label
mkLabels lst = M.fromList $ map (Data.Bifunctor.second mkLabel) lst

mkVar :: VarName -> Type
mkVar s = TyVar $ Ident s
