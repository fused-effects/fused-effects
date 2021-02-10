module NonDet.Church
( tests
) where

import Control.Carrier.Error.Either
import Control.Carrier.NonDet.Church
import Control.Carrier.State.Strict hiding (state)
import Hedgehog
import Prelude hiding (error)
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "NonDet.Church"
  [ testProperty "collects results of effects run inside it" . property $
    run (runNonDetA (runState 'a' state))
    === [('a', 'z'), ('b', 'b'), ('a', 'a')]
  , testProperty "collapses results of effects run outside it" . property $
    run (runState 'a' (runNonDetA state))
    === ('b', "zbb")
  , testProperty "collects results from higher-order effects run inside it" . property $
    run (runNonDetA (runError error))
    === [Right 'z', Right 'a' :: Either Char Char]
  , testProperty "collapses results of higher-order effects run outside it" . property $
    run (runError (runNonDetA error))
    === (Right "a" :: Either Char String)
  ]

state :: (Alternative m, Has (State Char) sig m) => m Char
state = pure 'z' <|> put 'b' *> get <|> get

error :: (Alternative m, Has (Error Char) sig m) => m Char
error = (pure 'z' <|> throwError 'a') `catchError` pure
