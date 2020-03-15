{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
module Control.Carrier.Empty.Church
( -- * Empty carrier
  EmptyC(..)
  -- * Empty effect
, module Control.Effect.Empty
) where

import Control.Effect.Empty

newtype EmptyC m a = EmptyC (forall b . (a -> m b) -> m b -> m b)
  deriving (Functor)

instance Applicative (EmptyC m) where
  pure a = EmptyC $ \ leaf _ -> leaf a
  {-# INLINE pure #-}

  EmptyC f <*> EmptyC a = EmptyC $ \ leaf nil ->
    f (\ f' -> a (leaf . f') nil) nil
  {-# INLINE (<*>) #-}
