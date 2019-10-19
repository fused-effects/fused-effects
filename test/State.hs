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
  [ test "StateC (Lazy)"   LazyStateC.runState
  , test "StateC (Strict)" StrictStateC.runState
  , test "StateT (Lazy)"   (fmap (fmap swap) . flip LazyStateT.runStateT)
  , test "StateT (Strict)" (fmap (fmap swap) . flip StrictStateT.runStateT)
  , test "RWST (Lazy)"     (runRWST LazyRWST.runRWST)
  , test "RWST (Strict)"   (runRWST StrictRWST.runRWST)
  ] where
  test :: Has (State S) sig m => String -> (forall a . S -> m a -> PureC (S, a)) -> TestTree
  test name run = testGroup name
    $  Monad.test   (m (gen s)) a b c ((,) <$> s <*> pure ()) (uncurry run)
    ++ State.test s (m (gen s)) a                             run
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
