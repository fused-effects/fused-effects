{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module State
( tests
, gen
, stateTests
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
import Hedgehog.Function hiding (S)
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
  stateTests :: Has (State S) sig m => (forall a . (S -> m a -> PureC (S, a))) -> [TestTree]
  stateTests run = State.stateTests run (genM (gen s)) s a
  runRWST f s m = (\ (a, s, ()) -> (s, a)) <$> f m s s


gen :: forall s m a sig . (Has (State s) sig m, Arg s, Show a, Show s, Vary s) => Gen s -> (forall a . Show a => Gen a -> Gen (With (m a))) -> Gen a -> Gen (With (m a))
gen s _ a = choice
  [ liftWith "gets" (gets @s) . showingFn <$> fn a
  , liftWith2 "(<$)" (<$) . showing <$> a <*> (liftWith "put" put . showing <$> s)
  ]


stateTests :: (Has (State s) sig m, Arg s, Eq a, Eq s, Show a, Show s, Vary s) => (forall a . (s -> m a -> PureC (s, a))) -> (forall a . Show a => Gen a -> Gen (With (m a))) -> Gen s -> Gen a -> [TestTree]
stateTests runState m s a =
  [ testProperty "get state" . forall (s :. fn (m a) :. Nil) $
    \ s (FnWith k) -> get_state (===) runState s k
  , testProperty "put update" . forall (s :. s :. m a :. Nil) $
    \ s s' (With m) -> put_update (===) runState s s' m
  ]
