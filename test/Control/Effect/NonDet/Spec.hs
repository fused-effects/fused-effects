module Control.Effect.NonDet.Spec
( spec
) where

import Control.Carrier.Error.Either
import Control.Carrier.NonDet.Church
import Control.Carrier.State.Strict
import Test.Hspec

spec :: Spec
spec = do
  describe "interactions" $ do
    it "collects results of effects run before it" $
      run (runNonDet (runState 'a' (pure 'z' <|> put 'b' *> get <|> get))) `shouldBe` [('a', 'z'), ('b', 'b'), ('a', 'a')]

    it "collapses results of effects run after it" $
      run (runState 'a' (runNonDet (pure 'z' <|> put 'b' *> get <|> get))) `shouldBe` ('b', "zbb")

    it "collects results from higher-order effects run before it" $
      run (runNonDet (runError ((pure 'z' <|> throwError 'a') `catchError` pure))) `shouldBe` [Right 'z', Right 'a' :: Either Char Char]

    it "collapses results of higher-order effects run after it" $
      run (runError (runNonDet ((pure 'z' <|> throwError 'a') `catchError` pure))) `shouldBe` (Right "a" :: Either Char String)
