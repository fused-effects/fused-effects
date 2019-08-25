{-# LANGUAGE DeriveFunctor, RankNTypes #-}
module Control.Effect.Error.CPS
( -- * Error effect
  module Control.Effect.Error
  -- * Error carrier
, ErrorC(..)
) where

import Control.Effect.Error (Error, throwError, catchError)

newtype ErrorC e m a = ErrorC { runErrorC :: forall b . (e -> m b) -> (a -> m b) -> m b }
  deriving (Functor)

instance Applicative (ErrorC e m) where
  pure a = ErrorC $ \ _ k -> k a
  ErrorC f <*> ErrorC a = ErrorC $ \ h k -> f h (\ f' -> a h (k . f'))

instance Monad (ErrorC e m) where
  ErrorC a >>= f = ErrorC $ \ h k -> a h (\ a' -> runErrorC (f a') h k)
