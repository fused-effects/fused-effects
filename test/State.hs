{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module State
( tests
) where

import qualified Control.Carrier.State.Lazy as LazyStateC
import qualified Control.Carrier.State.Strict as StrictStateC
import Control.Effect.State
import qualified Control.Monad.Trans.State.Lazy as LazyStateT
import qualified Control.Monad.Trans.State.Strict as StrictStateT
import Data.Tuple (swap)
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "State"
  [ testState "StateC (Lazy)"   LazyStateC.runState   genA
  , testState "StateC (Strict)" StrictStateC.runState genA
  , testState "StateT (Lazy)"   (fmap (fmap swap) . flip LazyStateT.runStateT)   genA
  , testState "StateT (Strict)" (fmap (fmap swap) . flip StrictStateT.runStateT) genA
  ]


genState :: Has (State a) sig m => Gen a -> Gen (m a) -> Gen (m a)
genState a _ = choice [ pure get, put' <$> a ] where
  put' a = a <$ put a


testState :: (Has (State s) sig m, Arg s, Eq s, Show s, Vary s) => String -> (forall a . (s -> m a -> PureC (s, a))) -> Gen s -> TestTree
testState name runState gen = testGroup name
  [ testProperty "get state" . forall (gen :. fn (Blind <$> genM [genState] gen) :. Nil) $
    \ a k -> get_state (~=) runState a (getBlind . apply k)
  , testProperty "put update" . forall (gen :. gen :. fmap Blind (genM [genState] gen) :. Nil) $
    \ a b m -> put_update (~=) runState a b (getBlind m)
  ]
