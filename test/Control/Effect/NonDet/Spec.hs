module Control.Effect.NonDet.Spec where

import Control.Effect
import Test.Hspec

spec :: Spec
spec = do
  describe "empty" $ do
    it "produces no values" $
      run (runNonDet empty) `shouldBe` ([] :: String)
