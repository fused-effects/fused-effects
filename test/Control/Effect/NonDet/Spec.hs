module Control.Effect.NonDet.Spec
( spec
, tests
) where

import Control.Carrier.Error.Either
import Control.Carrier.NonDet.Church
import Control.Carrier.State.Strict
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "NonDet"
  [ interactions
  ]

interactions :: TestTree
interactions = testGroup "interactions"
  [ testCase "collects results of effects run before it" $
    run (runNonDet (runState 'a' (pure 'z' <|> put 'b' *> get <|> get)))
    @?= [('a', 'z'), ('b', 'b'), ('a', 'a')]
  , testCase "collapses results of effects run after it" $
    run (runState 'a' (runNonDet (pure 'z' <|> put 'b' *> get <|> get)))
    @?= ('b', "zbb")
  , testCase "collects results from higher-order effects run before it" $
    run (runNonDet (runError ((pure 'z' <|> throwError 'a') `catchError` pure)))
    @?= [Right 'z', Right 'a' :: Either Char Char]
  , testCase "collapses results of higher-order effects run after it" $
    run (runError (runNonDet ((pure 'z' <|> throwError 'a') `catchError` pure)))
    @?= (Right "a" :: Either Char String)
  ]

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
