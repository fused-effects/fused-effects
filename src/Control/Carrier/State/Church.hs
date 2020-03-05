{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
module Control.Carrier.State.Church
( -- * State carrier
  StateC(StateC)
  -- * State effect
, module Control.Effect.State
) where

import Control.Effect.State

newtype StateC s m a = StateC (forall r . (a -> s -> m r) -> s -> m r)
  deriving (Functor)

instance Applicative (StateC s m) where
  pure a = StateC $ \ k s -> k a s

  StateC f <*> StateC a = StateC $ \ k -> f (a . (k .))
