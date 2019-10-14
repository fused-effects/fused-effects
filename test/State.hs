{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module State
( tests
) where

import qualified Control.Carrier.State.Lazy as LazyStateC
import qualified Control.Carrier.State.Strict as StrictStateC
import Control.Effect.State
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWST
import qualified Control.Monad.Trans.RWS.Strict as StrictRWST
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
  [ testGroup "StateC (Lazy)"   $ stateTests LazyStateC.runState
  , testGroup "StateC (Strict)" $ stateTests StrictStateC.runState
  , testGroup "StateT (Lazy)"   $ stateTests (fmap (fmap swap) . flip LazyStateT.runStateT)
  , testGroup "StateT (Strict)" $ stateTests (fmap (fmap swap) . flip StrictStateT.runStateT)
  , testGroup "RWST (Lazy)"     $ stateTests (runRWST LazyRWST.runRWST)
  , testGroup "RWST (Strict)"   $ stateTests (runRWST StrictRWST.runRWST)
  ] where
  stateTests :: Has (State A) sig m => (forall a . (A -> m a -> PureC (A, a))) -> [TestTree]
  stateTests run = State.stateTests run genA
  runRWST f s m = (\ (a, s, ()) -> (s, a)) <$> f m s s


genState :: Has (State a) sig m => Gen a -> Gen (m a) -> Gen (m a)
genState a _ = choice [ pure get, put' <$> a ] where
  put' a = a <$ put a


stateTests :: (Has (State s) sig m, Arg s, Eq s, Show s, Vary s) => (forall a . (s -> m a -> PureC (s, a))) -> Gen s -> [TestTree]
stateTests runState s =
  [ testProperty "get state" . forall (s :. fn (genM [genState] s) :. Nil) $
    \ a k -> get_state (~=) runState a (getBlind . apply k)
  , testProperty "put update" . forall (s :. s :. genM [genState] s :. Nil) $
    \ a b m -> put_update (~=) runState a b (getBlind m)
  ]
