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
import qualified Monad
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "State"
  [ testGroup "StateC (Lazy)"   $
    [ testMonad
    , testState
    ] >>= ($ RunS LazyStateC.runState)
  , testGroup "StateC (Strict)" $
    [ testMonad
    , testState
    ] >>= ($ RunS StrictStateC.runState)
  , testGroup "StateT (Lazy)"   $ testState (RunS (fmap (fmap swap) . flip LazyStateT.runStateT))
  , testGroup "StateT (Strict)" $ testState (RunS (fmap (fmap swap) . flip StrictStateT.runStateT))
  , testGroup "RWST (Lazy)"     $ testState (RunS (runRWST LazyRWST.runRWST))
  , testGroup "RWST (Strict)"   $ testState (RunS (runRWST StrictRWST.runRWST))
  ] where
  testMonad (RunS run) = Monad.test   (m (gen s)) a b c ((,) <$> s <*> pure ()) (uncurry run)
  testState (RunS run) = State.test s (m (gen s)) a                                      run
  runRWST f s m = (\ (a, s, ()) -> (s, a)) <$> f m s s


gen
  :: forall s m a sig
  .  (Has (State s) sig m, Arg s, Show s, Vary s)
  => Gen s
  -> (forall a . Gen a -> Gen (m a))
  -> Gen a
  -> Gen (m a)
gen s _ a = choice
  [ label "gets" (gets @s) <*> fn a
  , infixL 4 "<$" (<$) <*> a <*> (label "put" put <*> s)
  ]


test
  :: (Has (State s) sig m, Arg s, Eq a, Eq s, Show a, Show s, Vary s)
  => Gen s
  -> (forall a . Gen a -> Gen (m a))
  -> Gen a
  -> (forall a . s -> m a -> PureC (s, a))
  -> [TestTree]
test s m a runState =
  [ testProperty "get returns the state variable" . forall (s :. fn (m a) :. Nil) $
    \ s k -> runState s (get >>= k) === runState s (k s)
  , testProperty "put updates the state variable" . forall (s :. s :. m a :. Nil) $
    \ s s' m -> runState s (put s' >> m) === runState s' m
  ]
