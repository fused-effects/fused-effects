module Control.Effect.NonDet.Spec where

import Control.Effect
import Test.Hspec

spec :: Spec
spec = do
  describe "empty" $ do
    it "produces no values" $
      run (runNonDet empty) `shouldBe` ([] :: String)

  describe "<|>" $ do
    it "produces each branch’s result nondeterministically" $
      run (runNonDet (pure 'a' <|> pure 'b')) `shouldBe` "ab"
