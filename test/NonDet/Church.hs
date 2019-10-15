{-# LANGUAGE FlexibleContexts #-}
module NonDet.Church
( tests
) where

import Control.Carrier.Error.Either
import Control.Carrier.NonDet.Church
import Control.Carrier.State.Strict
import Prelude hiding (error)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "NonDet.Church"
  [ testCase "collects results of effects run inside it" $
    run (runNonDetA (runState 'a' state))
    @?= [('a', 'z'), ('b', 'b'), ('a', 'a')]
  , testCase "collapses results of effects run outside it" $
    run (runState 'a' (runNonDetA state))
    @?= ('b', "zbb")
  , testCase "collects results from higher-order effects run inside it" $
    run (runNonDetA (runError error))
    @?= [Right 'z', Right 'a' :: Either Char Char]
  , testCase "collapses results of higher-order effects run outside it" $
    run (runError (runNonDetA error))
    @?= (Right "a" :: Either Char String)
  ]

state :: (Alternative m, Has (State Char) sig m) => m Char
state = pure 'z' <|> put 'b' *> get <|> get

error :: (Alternative m, Has (Error Char) sig m) => m Char
error = (pure 'z' <|> throwError 'a') `catchError` pure
