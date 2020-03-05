{-# LANGUAGE RankNTypes #-}
module Control.Carrier.State.Church
( -- * State carrier
  StateC(StateC)
) where

newtype StateC s m a = StateC { runStateC :: forall r . (a -> s -> m r) -> s -> m r }
