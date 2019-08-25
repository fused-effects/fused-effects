{-# LANGUAGE DeriveFunctor, RankNTypes #-}
module Control.Effect.Error.CPS
( -- * Error effect
  module Control.Effect.Error
  -- * Error carrier
, runError
, ErrorC(..)
) where

import Control.Effect.Error (Error, throwError, catchError)
import Control.Monad.Trans.Class

runError :: (e -> m b) -> (a -> m b) -> ErrorC e m a -> m b
runError h k m = runErrorC m h k

newtype ErrorC e m a = ErrorC { runErrorC :: forall b . (e -> m b) -> (a -> m b) -> m b }
  deriving (Functor)

instance Applicative (ErrorC e m) where
  pure a = ErrorC $ \ _ k -> k a
  ErrorC f <*> ErrorC a = ErrorC $ \ h k -> f h (\ f' -> a h (k . f'))

instance Monad (ErrorC e m) where
  ErrorC a >>= f = ErrorC $ \ h k -> a h (runError h k . f)

instance MonadTrans (ErrorC e) where
  lift m = ErrorC $ \ _ k -> m >>= k
