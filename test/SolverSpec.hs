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
  describe "SolverSpec" $ do
    it "a0 <= [a100]" $ do
      let cs1 = [ mkGtLabels "a0" [a100] ]
      res <- Data.Either.fromRight (error "") <$> solve em1 cs1
      res `shouldBe` [("a0", [("A", v100), ("B", tbd)])]
    it "a0 <= [a100, a101]" $ do
      let cs1 = [ mkGtLabels "a0" [a100,a101] ]
      res <- Data.Either.fromRight (error "") <$> solve em1 cs1
      res `shouldBe` [("a0", [("A", v101), ("B", tbd)])]
    it "a0 <= [a100, a101]" $ do
      let cs1 = [ mkGtLabels "a0" [a100,a101] ]
      res <- Data.Either.fromRight (error "") <$> solve em1 cs1
      res `shouldBe` [("a0", [("A", v101), ("B", tbd)])]

-------------------

mkGtLabels :: String -> [Label] -> Coeffect
mkGtLabels vn labels = CSubset (mkVar vn) (mkTyLabels labels)

mkGtVar :: String -> String -> Coeffect
mkGtVar vn1 vn2 = CSubset (mkVar vn1) (mkVar vn2)

mkTyLabels :: [Label] -> Type
mkTyLabels ls = TyLabels $ Data.Set.fromList ls

em1 :: Data.Map.Map String [Version]
em1 = Data.Map.fromList
  [ ("A",
    [ Version 1 0 0
    , Version 1 0 1])
  , ("B",
    [ Version 1 0 0
    , Version 1 0 1])
  ]

a100, a101, b100, b101 :: Label
a100 = mkLabel [("A", v100)]
a101 = mkLabel [("A", v101)]
b100 = mkLabel [("B", v100)]
b101 = mkLabel [("B", v101)]

v100, v101, tbd :: Version
v100 = mkVer 1 0 0
v101 = mkVer 1 0 1
tbd  = TBD