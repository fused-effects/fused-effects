{-# LANGUAGE DataKinds, FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module State
( tests
, gen0
, test
) where

import qualified Control.Carrier.State.Lazy as LazyStateC
import qualified Control.Carrier.State.Strict as StrictStateC
import           Control.Effect.State
import           Control.Exception (SomeException, evaluate, try)
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWST
import qualified Control.Monad.Trans.RWS.Strict as StrictRWST
import qualified Control.Monad.Trans.State.Lazy as LazyStateT
import qualified Control.Monad.Trans.State.Strict as StrictStateT
import           Data.Either
import           Data.Tuple (swap)
import           Gen
import           Hedgehog (assert, eval)
import qualified Monad
import qualified MonadFix
import           Test.Tasty
import           Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "State"
  [ testGroup "StateC (Lazy)"   $
    [ testMonad
    , testMonadFix
    , testState
    , testLazy
    ] >>= ($ runC LazyStateC.runState)
  , testGroup "StateC (Strict)" $
    [ testMonad
    , testMonadFix
    , testState
    , testStrict
    ] >>= ($ runC StrictStateC.runState)
  , testGroup "StateT (Lazy)"   $ testState (runC (fmap (fmap swap) . flip LazyStateT.runStateT))
  , testGroup "StateT (Strict)" $ testState (runC (fmap (fmap swap) . flip StrictStateT.runStateT))
  , testGroup "RWST (Lazy)"     $ testState (runC (runRWST LazyRWST.runRWST))
  , testGroup "RWST (Strict)"   $ testState (runC (runRWST StrictRWST.runRWST))
  ] where
  testMonad    run = Monad.test    (m (gen0 s) (\ _ _ -> [])) a b c (pair <*> s <*> unit) run
  testMonadFix run = MonadFix.test (m (gen0 s) (\ _ _ -> [])) a b   (pair <*> s <*> unit) run
  testState    run = State.test    (m (gen0 s) (\ _ _ -> [])) a               s           run
  testLazy     run = lazy                                     a     (pair <*> s <*> unit) run
  testStrict   run = strict                                   a     (pair <*> s <*> unit) run
  runRWST f s m = (\ (a, s, ()) -> (s, a)) <$> f m s s


gen0
  :: forall s m a sig
  .  (Has (State s) sig m, Arg s, Show s, Vary s)
  => GenTerm s
  -> GenTerm a
  -> [GenTerm (m a)]
gen0 s a =
  [ label "gets" (gets @s) <*> fn a
  , infixL 4 "<$" (<$) <*> a <*> (label "put" put <*> s)
  ]


test
  :: (Has (State s) sig m, Arg s, Eq a, Eq s, Show a, Show s, Vary s)
  => GenM m
  -> GenTerm a
  -> GenTerm s
  -> Run ((,) s) ((,) s) m
  -> [TestTree]
test m a s (Run runState) =
  [ testProperty "get returns the state variable" . forall (s :. fn (m a) :. Nil) $
    \ s k -> runState (s, get >>= k) === runState (s, k s)
  , testProperty "put updates the state variable" . forall (s :. s :. m a :. Nil) $
    \ s s' m -> runState (s, put s' >> m) === runState (s', m)
  ]

problematic :: Applicative f => f a
problematic = (pure (error "should immediately")) <*> pure (error "terminate")

lazy
  :: forall m a f g . (Monad m, Functor f)
  => GenTerm a
  -> GenTerm (f ())
  -> Run f g m
  -> [TestTree]
lazy _a s (Run run) =
  [ testProperty "fmap is non-strict" . forall (a :. s :. Nil) $
    \ a s -> void . eval . run $ (const a <$> pure (error "insufficiently lazy")) <$ s
  , testProperty "pure and <*> are non-strict" . forall (s :. Nil) $
    \ s -> void . eval . run $ problematic <$ s
  ]

strict
  :: forall m a f g . (Traversable f, Monad m)
  => GenTerm a
  -> GenTerm (f ())
  -> Run f g m
  -> [TestTree]
strict _a s (Run run) =
  [ testProperty "pure and <*> are strict" . forall (s :. Nil) $
    \ s -> (liftIO . try @SomeException . evaluate . run $ (problematic <$ s)) >>= assert . isLeft
  ]
