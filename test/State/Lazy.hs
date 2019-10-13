{-# LANGUAGE TypeApplications #-}
module State.Lazy
( tests
) where

import Control.Carrier.State.Lazy
import Hedgehog.Function
import Pure
import qualified State
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "State.Lazy.StateC"
  [ testProperty "get state" . forall (genA :. fn @A (Blind <$> State.gen genA) :. Nil) $
    \ a k -> get_state (~=) runState a (getBlind . apply k)
  , testProperty "put update" . forall (genA :. genA :. fmap Blind (State.gen genA) :. Nil) $
    \ a b m -> put_update (~=) runState a b (getBlind m)
  ]
