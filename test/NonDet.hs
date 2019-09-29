module NonDet
( tests
) where

import Control.Carrier.Error.Either
import Control.Carrier.NonDet.Church
import Control.Carrier.State.Strict
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "NonDet"
  [ testCase "collects results of effects run inside it" $
    run (runNonDet (runState 'a' (pure 'z' <|> put 'b' *> get <|> get)))
    @?= [('a', 'z'), ('b', 'b'), ('a', 'a')]
  , testCase "collapses results of effects run outside it" $
    run (runState 'a' (runNonDet (pure 'z' <|> put 'b' *> get <|> get)))
    @?= ('b', "zbb")
  , testCase "collects results from higher-order effects run inside it" $
    run (runNonDet (runError ((pure 'z' <|> throwError 'a') `catchError` pure)))
    @?= [Right 'z', Right 'a' :: Either Char Char]
  , testCase "collapses results of higher-order effects run outside it" $
    run (runError (runNonDet ((pure 'z' <|> throwError 'a') `catchError` pure)))
    @?= (Right "a" :: Either Char String)
  ]
