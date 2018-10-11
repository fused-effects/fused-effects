module Control.Effect.Reader.Spec where

import Control.Effect
import Test.Hspec

spec :: Spec
spec = do
  describe "ask" $ do
    it "produces a value" $
      run (runReader "i" ((++) . ('h' :) <$> ask <*> ask)) `shouldBe` "hii"

  describe "local" $ do
    it "modifies the environment with a function" $
      run (runReader (0 :: Int) (local (succ :: Int -> Int) ask)) `shouldBe` (1 :: Int)
