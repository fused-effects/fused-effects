{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | Labelled 'State' operations.
--
-- @since 1.0.2.0
module Control.Effect.State.Labelled
( -- * State effect
  State
, get
, gets
, put
, modify
, modifyLazy
, state
  -- * Re-exports
, Algebra
, Has
, HasLabelled
, run
) where

import           Control.Effect.Labelled
import qualified Control.Effect.State as S
import           Control.Effect.State.Internal

-- | Get the current state value.
--
-- @
-- runState a ('runLabelled' @label ('get' @label) '>>=' k) = runState a (k a)
-- @
--
-- @since 1.0.2.0
get :: forall label s m sig . HasLabelled label (State s) sig m => m s
get = runUnderLabel @label S.get
{-# INLINEABLE get #-}

-- | Project a function out of the current state value.
--
-- @
-- 'gets' f = 'fmap' f 'get'
-- @
--
-- @since 1.0.2.0
gets :: forall label s m a sig . HasLabelled label (State s) sig m => (s -> a) -> m a
gets f = runUnderLabel @label (S.gets f)
{-# INLINEABLE gets #-}

-- | Replace the state value with a new value.
--
-- @
-- runState a ('runLabelled' @label ('put' @label b) '>>' m) = runState b m
-- @
--
-- @since 1.0.2.0
put :: forall label s m sig . HasLabelled label (State s) sig m => s -> m ()
put s = runUnderLabel @label (S.put s)
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
modify f = runUnderLabel @label (S.modify f)
{-# INLINEABLE modify #-}

-- | Replace the state value with the result of applying a function to the current state value.
--   This is lazy in the new state; injudicious use of this function may lead to space leaks.
--
-- @
-- 'modifyLazy' f = 'get' '>>=' 'put' . f
-- @
--
-- @since 1.0.2.0
modifyLazy :: forall label s m sig . HasLabelled label (State s) sig m => (s -> s) -> m ()
modifyLazy f = runUnderLabel @label (S.modifyLazy f)
{-# INLINEABLE modifyLazy #-}

-- | Compute a new state and a value in a single step.
--
-- @
-- 'state' f = 'gets' f '>>=' \\ (s, a) -> 'put' s '>>' 'pure' a
-- @
--
-- @since 1.0.2.0
state :: forall label s m a sig . HasLabelled label (State s) sig m => (s -> (s, a)) -> m a
state f = runUnderLabel @label (S.state f)
{-# INLINEABLE state #-}
