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
