{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Control.Effect.State.Labelled
( -- * State effect
  State
, get
, gets
, put
, modify
  -- * Re-exports
, Algebra
, Effect
, Has
, run
) where

import           Control.Effect.Labelled
import qualified Control.Effect.State as S
import           Control.Effect.State.Internal

-- | Get the current state value.
--
-- @
-- runState a ('runLabelled' @_ @label ('get' @label) '>>=' k) = runState a (k a)
-- @
--
-- @since 1.0.2.0
get :: forall label s m sig . HasLabelled label (State s) sig m => m s
get = runUnderLabel @_ @label S.get
{-# INLINEABLE get #-}

-- | Project a function out of the current state value.
--
-- @
-- 'gets' f = 'fmap' f 'get'
-- @
--
-- @since 1.0.2.0
gets :: forall label s m a sig . HasLabelled label (State s) sig m => (s -> a) -> m a
gets f = runUnderLabel @_ @label (S.gets f)
{-# INLINEABLE gets #-}

-- | Replace the state value with a new value.
--
-- @
-- runState a ('runLabelled' @_ @label ('put' @label b) '>>' m) = runState b m
-- @
--
-- @since 1.0.2.0
put :: forall label s m sig . HasLabelled label (State s) sig m => s -> m ()
put s = runUnderLabel @_ @label (S.put s)
{-# INLINEABLE put #-}

-- | Replace the state value with the result of applying a function to the current state value.
--   This is strict in the new state.
--
-- @
-- 'modify' f = 'get' '>>=' ('put' . f '$!')
-- @
--
-- @since 1.0.2.0
modify :: forall label s m sig . HasLabelled label (State s) sig m => (s -> s) -> m ()
modify f = runUnderLabel @_ @label (S.modify f)
{-# INLINEABLE modify #-}
