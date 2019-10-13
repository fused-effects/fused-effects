{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module State.Gen
( genState
, testState
) where

import Control.Effect.State
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

genState :: Has (State a) sig m => Gen a -> Gen (m a) -> Gen (m a)
genState a _ = choice [ pure get, put' <$> a ] where
  put' a = a <$ put a


testState :: (Has (State s) sig m, Arg s, Eq s, Show s, Vary s) => String -> (forall a . (s -> m a -> PureC (s, a))) -> Gen s -> TestTree
testState name runState gen = testGroup name
  [ testProperty "get state" . forall (gen :. fn (Blind <$> genM genState gen) :. Nil) $
    \ a k -> get_state (~=) runState a (getBlind . apply k)
  , testProperty "put update" . forall (gen :. gen :. fmap Blind (genM genState gen) :. Nil) $
    \ a b m -> put_update (~=) runState a b (getBlind m)
  ]
