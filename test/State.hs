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
import qualified MonadFix
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "State"
  [ testGroup "StateC (Lazy)"   $
    [ testMonad
    , testMonadFix
    , testState
    ] >>= ($ RunS LazyStateC.runState)
  , testGroup "StateC (Strict)" $
    [ testMonad
    , testMonadFix
    , testState
    ] >>= ($ RunS StrictStateC.runState)
  , testGroup "StateT (Lazy)"   $ testState (RunS (fmap (fmap swap) . flip LazyStateT.runStateT))
  , testGroup "StateT (Strict)" $ testState (RunS (fmap (fmap swap) . flip StrictStateT.runStateT))
  , testGroup "RWST (Lazy)"     $ testState (RunS (runRWST LazyRWST.runRWST))
  , testGroup "RWST (Strict)"   $ testState (RunS (runRWST StrictRWST.runRWST))
  ] where
  testMonad    run = Monad.test    (m (gen s)) a b c (atom "(,)" (,) <*> s <*> unit) run
  testMonadFix run = MonadFix.test (m (gen s)) a b   (atom "(,)" (,) <*> s <*> unit) run
  testState    run = State.test s  (m (gen s)) a                                     run
  runRWST f s m = (\ (a, s, ()) -> (s, a)) <$> f m s s


gen
  :: forall s m sig
  .  (Has (State s) sig m, Arg s, Show s, Vary s)
  => Gen s
  -> GenM m
  -> GenM m
gen s _ a = choice
  [ label "gets" (gets @s) <*> fn a
  , infixL 4 "<$" (<$) <*> a <*> (label "put" put <*> s)
  ]


test
  :: (Has (State s) sig m, Arg s, Eq a, Eq s, Show a, Show s, Vary s)
  => Gen s
  -> GenM m
  -> Gen a
  -> RunS s ((,) s) m
  -> [TestTree]
test s m a (RunS runState) =
  [ testProperty "get returns the state variable" . forall (s :. fn (m a) :. Nil) $
    \ s k -> runState s (get >>= k) === runState s (k s)
  , testProperty "put updates the state variable" . forall (s :. s :. m a :. Nil) $
    \ s s' m -> runState s (put s' >> m) === runState s' m
  ]
