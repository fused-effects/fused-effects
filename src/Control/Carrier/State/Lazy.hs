{-# LANGUAGE ExplicitForAll, PatternSynonyms, ViewPatterns #-}

{- | A carrier for the 'State' effect that refrains from evaluating its state until necessary. This is less efficient than "Control.Carrier.State.Strict" but allows some cyclic computations to terminate that would loop infinitely in a strict state carrier.

Note that the parameter order in 'runState', 'evalState', and 'execState' is reversed compared the equivalent functions provided by @transformers@. This is an intentional decision made to enable the composition of effect handlers with '.' without invoking 'flip'.

@since 1.0.0.0
-}

module Control.Carrier.State.Lazy
( -- * Lazy state carrier
  runState
, evalState
, execState
, StateC
, pattern StateC
  -- * State effect
, module Control.Effect.State
) where

import Control.Algebra
import Control.Effect.State
import qualified Control.Monad.Trans.State.Lazy as T
import Data.Tuple (swap)

-- | Run a lazy 'State' effect, yielding the result value and the final state. More programs terminate with lazy state than strict state, but injudicious use of lazy state may lead to thunk buildup.
--
-- @
-- 'runState' s ('pure' a) = 'pure' (s, a)
-- @
-- @
-- 'runState' s 'get' = 'pure' (s, s)
-- @
-- @
-- 'runState' s ('put' t) = 'pure' (t, ())
-- @
--
-- @since 1.0.0.0
runState :: forall s m a . Functor m => s -> StateC s m a -> m (s, a)
runState s (StateC runStateC) = runStateC s
{-# INLINE[3] runState #-}

-- | Run a lazy 'State' effect, yielding the result value and discarding the final state.
--
-- @
-- 'evalState' s m = 'fmap' 'snd' ('runState' s m)
-- @
--
-- @since 1.0.0.0
evalState :: forall s m a . Functor m => s -> StateC s m a -> m a
evalState s = fmap snd . runState s
{-# INLINE[3] evalState #-}

-- | Run a lazy 'State' effect, yielding the final state and discarding the return value.
--
-- @
-- 'execState' s m = 'fmap' 'fst' ('runState' s m)
-- @
--
-- @since 1.0.0.0
execState :: forall s m a . Functor m => s -> StateC s m a -> m s
execState s = fmap fst . runState s
{-# INLINE[3] execState #-}

-- | @since 1.0.0.0
type StateC = T.StateT

pattern StateC :: Functor m => (s -> m (s, a)) -> StateC s m a
pattern StateC run <- T.StateT (fmap (fmap swap) -> run) where
  StateC run = T.StateT (fmap swap . run)

{-# COMPLETE StateC #-}
