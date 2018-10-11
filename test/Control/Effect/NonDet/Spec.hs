module Control.Effect.NonDet.Spec where

import Control.Effect
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "empty" $ do
    it "produces no values" $
      run (runNonDet empty) `shouldBe` ([] :: String)

  describe "<|>" $ do
    it "produces each branch’s result nondeterministically" $
      run (runNonDet (pure 'a' <|> pure 'b')) `shouldBe` "ab"

  describe "laws" $ do
    prop "associativity" $
      \ a b c -> run (runNonDet ((pure a <|> pure b) <|> pure c)) `shouldBe` run (runNonDet (pure a <|> (pure b <|> pure (c :: Char))))

    prop "left-identity" $
      \ b -> run (runNonDet (empty <|> pure b)) `shouldBe` [b :: Char]

    prop "right-identity" $
      \ a -> run (runNonDet (pure a <|> empty)) `shouldBe` [a :: Char]

  describe "interactions" $ do
    it "collects results of effects run before it" $
      run (runNonDet (runState 'a' (pure 'z' <|> put 'b' *> get <|> get))) `shouldBe` [('a', 'z'), ('b', 'b'), ('a', 'a')]
