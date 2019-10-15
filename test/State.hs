{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module State
( tests
, gen
, test
) where

import qualified Control.Carrier.State.Lazy as LazyStateC
import qualified Control.Carrier.State.Strict as StrictStateC
import Control.Effect.State
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWST
import qualified Control.Monad.Trans.RWS.Strict as StrictRWST
import qualified Control.Monad.Trans.State.Lazy as LazyStateT
import qualified Control.Monad.Trans.State.Strict as StrictStateT
import Data.Tuple (swap)
import Gen
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "State"
  [ testGroup "StateC (Lazy)"   $ test s (m (gen s)) a LazyStateC.runState
  , testGroup "StateC (Strict)" $ test s (m (gen s)) a StrictStateC.runState
  , testGroup "StateT (Lazy)"   $ test s (m (gen s)) a (fmap (fmap swap) . flip LazyStateT.runStateT)
  , testGroup "StateT (Strict)" $ test s (m (gen s)) a (fmap (fmap swap) . flip StrictStateT.runStateT)
  , testGroup "RWST (Lazy)"     $ test s (m (gen s)) a (runRWST LazyRWST.runRWST)
  , testGroup "RWST (Strict)"   $ test s (m (gen s)) a (runRWST StrictRWST.runRWST)
  ] where
  runRWST f s m = (\ (a, s, ()) -> (s, a)) <$> f m s s


gen
  :: forall s m a sig
  .  (Has (State s) sig m, Arg s, Show a, Show s, Vary s)
  => Gen s
  -> (forall a . Show a => Gen a -> Gen (With (m a)))
  -> Gen a
  -> Gen (With (m a))
gen s _ a = choice
  [ liftWith "gets" (gets @s) . showingFn <$> fn a
  , liftWith2 "(<$)" (<$) . showing <$> a <*> (liftWith "put" put . showing <$> s)
  ]


test
  :: (Has (State s) sig m, Arg s, Eq a, Eq s, Show a, Show s, Vary s)
  => Gen s
  -> (forall a . Show a => Gen a -> Gen (With (m a)))
  -> Gen a
  -> (forall a . (s -> m a -> PureC (s, a)))
  -> [TestTree]
test s m a runState =
  [ testProperty "get returns the state variable" . forall (s :. fn (m a) :. Nil) $
    \ s (FnWith k) -> runState s (get >>= k) === runState s (k s)
  , testProperty "put updates the state variable" . forall (s :. s :. m a :. Nil) $
    \ s s' (With m) -> runState s (put s' >> m) === runState s' m
  ]
