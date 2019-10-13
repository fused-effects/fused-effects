{-# LANGUAGE TypeApplications #-}
module State.StateT.Lazy
( tests
) where

import Control.Effect.State
import qualified Control.Monad.Trans.State.Lazy as StateT
import Hedgehog.Function
import Pure
import State.Gen
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "State.StateT.Lazy"
  [ testProperty "get state" . forall (genA :. fn @A (Blind <$> genM genState genA) :. Nil) $
    \ a k -> get_state (~=) (flip StateT.runStateT) a (getBlind . apply k)
  , testProperty "put update" . forall (genA :. genA :. fmap Blind (genM genState genA) :. Nil) $
    \ a b m -> put_update (~=) (flip StateT.runStateT) a b (getBlind m)
  ]
