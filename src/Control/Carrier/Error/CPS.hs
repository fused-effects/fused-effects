{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
module Control.Carrier.Error.CPS
( -- * Error carrier
  ErrorC(..)
  -- * Error effect
, module Control.Effect.Error
) where

import Control.Effect.Error

newtype ErrorC e m a = ErrorC (forall b . (Either e a -> m b) -> m b)
  deriving (Functor)
