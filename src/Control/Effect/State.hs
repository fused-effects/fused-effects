{- | An effect that adds a mutable, updatable state value to a given computation.

Not all computations require a full-fledged state effect: read-only state is better served by 'Control.Effect.Reader.Reader', and append-only state without reads is better served by 'Control.Effect.Writer.Writer'.

Predefined carriers:

* "Control.Carrier.State.Strict", which is strict in its updates.
* "Control.Carrier.State.Lazy", which is lazy in its updates. This enables more programs to terminate, such as cyclic computations expressed with @MonadFix@ or @-XRecursiveDo@, at the cost of efficiency.
* "Control.Monad.Trans.RWS.Lazy"
* "Control.Monad.Trans.RWS.Strict"
* "Control.Monad.Trans.State.Lazy"
* "Control.Monad.Trans.State.Strict"

@since 0.1.0.0
-}

module Control.Effect.State
( -- * State effect
  State(..)
, get
, gets
, put
, modify
, modifyLazy
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import Control.Effect.State.Internal (State(..))

-- | Get the current state value.
--
-- @
-- runState a ('get' '>>=' k) = runState a (k a)
-- @
--
-- @since 0.1.0.0
get :: Has (State s) sig m => m s
get = send (Get pure)
{-# INLINEABLE get #-}

-- | Project a function out of the current state value.
--
-- @
-- 'gets' f = 'fmap' f 'get'
-- @
--
-- @since 0.1.0.0
gets :: Has (State s) sig m => (s -> a) -> m a
gets f = send (Get (pure . f))
{-# INLINEABLE gets #-}

-- | Replace the state value with a new value.
--
-- @
-- runState a ('put' b '>>' m) = runState b m
-- @
--
-- @since 0.1.0.0
put :: Has (State s) sig m => s -> m ()
put s = send (Put s (pure ()))
{-# INLINEABLE put #-}

-- | Replace the state value with the result of applying a function to the current state value.
--   This is strict in the new state.
--
-- @
-- 'modify' f = 'get' '>>=' ('put' . f '$!')
-- @
--
-- @since 0.1.0.0
modify :: Has (State s) sig m => (s -> s) -> m ()
modify f = do
  a <- get
  put $! f a
{-# INLINEABLE modify #-}

-- | Replace the state value with the result of applying a function to the current state value.
--   This is lazy in the new state; injudicious use of this function may lead to space leaks.
--
-- @
-- 'modifyLazy' f = 'get' '>>=' 'put' . f
-- @
--
-- @since 0.3.0.0
modifyLazy :: Has (State s) sig m => (s -> s) -> m ()
modifyLazy f = get >>= put . f
{-# INLINEABLE modifyLazy #-}
