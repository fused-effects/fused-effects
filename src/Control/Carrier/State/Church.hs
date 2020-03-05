{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
module Control.Carrier.State.Church
( -- * State carrier
  StateC(StateC)
  -- * State effect
, module Control.Effect.State
) where

import Control.Effect.State

newtype StateC s m a = StateC { runStateC :: forall r . (a -> s -> m r) -> s -> m r }
  deriving (Functor)
