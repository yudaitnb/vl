module PatternSynthesisSpec (spec) where

import Test.Hspec
import PatternSynthesis
import Syntax.Env

testPatSynth = patSynth 0 emptyUEnv emptyREnv

spec :: Spec
spec = do
  describe "patternSynthesis" $
    it "head" $
      head [1,2] `shouldBe` 1