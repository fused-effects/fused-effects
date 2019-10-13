{-# LANGUAGE TypeApplications #-}
module State.Strict
( tests
) where

import Control.Carrier.State.Strict
import Hedgehog.Function
import Pure
import State
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "State.Strict.StateC"
  [ testProperty "get state" . forall (genA :. fn @A (Blind <$> gen genA) :. Nil) $
    \ a k -> get_state (~=) runState a (getBlind . apply k)
  , testProperty "put update" . forall (genA :. genA :. fmap Blind (gen genA) :. Nil) $
    \ a b m -> put_update (~=) runState a b (getBlind m)
  ]
