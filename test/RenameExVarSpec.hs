module DuplicateExVarSpec (spec) where

import Test.Hspec
import Prelude

import qualified Data.Map as M
import Data.Map (fromList)
import qualified Data.Either

import Language.LambdaVL
import Translation.DuplicateExVars
import Parser
import Translation.Desugar (desugarAST)
import Translation.Girard (girardFwd)
import Translation.Normalize (normalize)

spec :: Spec
spec = do
  describe "DuplicateExVarSpec" $ do
    let code1 = "main = f 1 + f 2"
    it code1 $ getFVRenamed code1 `shouldBe` ["__f_0", "__f_1"]
    let code2 = "main = f a + f a"
    it code2 $ getFVRenamed code2 `shouldBe` ["__f_0", "__a_0", "__f_1", "__a_1"]
    let code3 = "main a = f a + f a"
    it code3 $ getFVRenamed code3 `shouldBe` ["__f_0", "__f_1"]
  where
    getFVRenamed = freeVars . getBind . head . getDecls . fst . (duplicateExVarModule M.empty) . getVLMod 
    getVLMod  =  girardFwd . normalize . desugarAST . parseString
