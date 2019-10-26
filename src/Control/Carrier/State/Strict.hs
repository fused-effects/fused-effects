{-# LANGUAGE ExplicitForAll, PatternSynonyms, ViewPatterns #-}

{- | A carrier for the 'State' effect. It evaluates its inner state strictly, which is the correct choice for the majority of use cases.

Note that the parameter order in 'runState', 'evalState', and 'execState' is reversed compared the equivalent functions provided by @transformers@. This is an intentional decision made to enable the composition of effect handlers with '.' without invoking 'flip'.

@since 1.0.0.0
-}
module Control.Carrier.State.Strict
( -- * Strict state carrier
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
import qualified Control.Monad.Trans.State.Strict as T
import Data.Tuple (swap)

-- | Run a 'State' effect starting from the passed value.
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
runState :: Functor m => s -> StateC s m a -> m (s, a)
runState s (StateC runStateC) = runStateC s
{-# INLINE[3] runState #-}

-- | Run a 'State' effect, yielding the result value and discarding the final state.
--
-- @
-- 'evalState' s m = 'fmap' 'snd' ('runState' s m)
-- @
--
-- @since 1.0.0.0
evalState :: forall s m a . Functor m => s -> StateC s m a -> m a
evalState s = fmap snd . runState s
{-# INLINE[3] evalState #-}

-- | Run a 'State' effect, yielding the final state and discarding the return value.
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
