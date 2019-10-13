{-# LANGUAGE TypeApplications #-}
module State.StateT.Strict
( tests
) where

import Control.Effect.State
import qualified Control.Monad.Trans.State.Strict as StateT
import Hedgehog.Function
import Pure
import qualified State
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "State.StateT.Strict"
  [ testProperty "get state" . forall (genA :. fn @A (Blind <$> State.gen genA) :. Nil) $
    \ a k -> get_state (~=) (flip StateT.runStateT) a (getBlind . apply k)
  , testProperty "put update" . forall (genA :. genA :. fmap Blind (State.gen genA) :. Nil) $
    \ a b m -> put_update (~=) (flip StateT.runStateT) a b (getBlind m)
  ]
