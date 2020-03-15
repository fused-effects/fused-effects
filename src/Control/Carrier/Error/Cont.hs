{-# LANGUAGE RankNTypes #-}
module Control.Carrier.Error.Cont
( -- * Error carrier
  ErrorC(..)
  -- * Error effect
, module Control.Effect.Error
) where

import Control.Effect.Error

newtype ErrorC e m a = ErrorC (forall b . (a -> m (Either e b)) -> m (Either e b))
