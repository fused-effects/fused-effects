{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module State
( tests
, gen0
, test
) where

import qualified Control.Carrier.State.Church as C.Church
import qualified Control.Carrier.State.Lazy as C.Lazy
import qualified Control.Carrier.State.Strict as C.Strict
import           Control.Effect.State
#if MIN_VERSION_transformers(0,5,6)
import qualified Control.Monad.Trans.RWS.CPS as RWST.CPS
#endif
import qualified Control.Monad.Trans.RWS.Lazy as RWST.Lazy
import qualified Control.Monad.Trans.RWS.Strict as RWST.Strict
import qualified Control.Monad.Trans.State.Lazy as T.Lazy
import qualified Control.Monad.Trans.State.Strict as T.Strict
import           Data.Tuple (swap)
import           Gen
import qualified Monad
import qualified MonadFix

tests :: TestTree
tests = testGroup "State"
  [ testGroup "StateC (Church)"   $
    [ testMonad
    , testMonadFix
    , testState
    ] >>= ($ runC (C.Church.runState (curry pure)))
  , testGroup "StateC (Lazy)"   $
    [ testMonad
    , testMonadFix
    , testState
    ] >>= ($ runC C.Lazy.runState)
  , testGroup "StateC (Strict)" $
    [ testMonad
    , testMonadFix
    , testState
    ] >>= ($ runC C.Strict.runState)
  , testGroup "StateT (Lazy)"   $ testState (runC (fmap (fmap swap) . flip T.Lazy.runStateT))
  , testGroup "StateT (Strict)" $ testState (runC (fmap (fmap swap) . flip T.Strict.runStateT))
#if MIN_VERSION_transformers(0,5,6)
  , testGroup "RWST (CPS)"      $ testState (runC (runRWST RWST.CPS.runRWST))
#endif
  , testGroup "RWST (Lazy)"     $ testState (runC (runRWST RWST.Lazy.runRWST))
  , testGroup "RWST (Strict)"   $ testState (runC (runRWST RWST.Strict.runRWST))
  ] where
  testMonad    run = Monad.test    (m (gen0 s) (\ _ _ -> [])) a b c (pair <*> s <*> unit) run
  testMonadFix run = MonadFix.test (m (gen0 s) (\ _ _ -> [])) a b   (pair <*> s <*> unit) run
  testState    run = State.test    (m (gen0 s) (\ _ _ -> [])) a               s           run
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
