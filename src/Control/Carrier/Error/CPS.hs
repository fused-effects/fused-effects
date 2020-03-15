{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
module Control.Carrier.Error.CPS
( -- * Error carrier
  runError
, ErrorC(..)
  -- * Error effect
, module Control.Effect.Error
) where

import Control.Applicative (liftA2)
import Control.Effect.Error
import Control.Monad.Fail as Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

runError :: (Either e a -> m b) -> ErrorC e m a -> m b
runError k (ErrorC m) = m k
{-# INLINE runError #-}

newtype ErrorC e m a = ErrorC (forall b . (Either e a -> m b) -> m b)
  deriving (Functor)

instance Applicative (ErrorC e m) where
  pure a = ErrorC $ \ k -> k (Right a)
  {-# INLINE pure #-}

  ErrorC f <*> ErrorC a = ErrorC $ \ k ->
    f (either (k . Left) (\ f' -> a (either (k . Left) (k . Right . f'))))
  {-# INLINE (<*>) #-}

  liftA2 f (ErrorC a) (ErrorC b) = ErrorC $ \ k ->
    a (either (k . Left) (\ a' -> b (either (k . Left) (k . Right . f a'))))
  {-# INLINE liftA2 #-}

  ErrorC a *> ErrorC b = ErrorC $ \ k ->
    a (either (k . Left) (const (b k)))
  {-# INLINE (*>) #-}

  ErrorC a <* ErrorC b = ErrorC $ \ k ->
    a (either (k . Left) (b . const . k . Right))
  {-# INLINE (<*) #-}

instance Monad (ErrorC e m) where
  ErrorC a >>= f = ErrorC $ \ k -> a (either (k . Left) (runError k . f))
  {-# INLINE (>>=) #-}

  (>>) = (*>)
  {-# INLINE (>>) #-}

instance Fail.MonadFail m => Fail.MonadFail (ErrorC e m) where
  fail = lift . Fail.fail
  {-# INLINE fail #-}

instance MonadIO m => MonadIO (ErrorC e m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance MonadTrans (ErrorC e) where
  lift m = ErrorC $ \ k -> m >>= k . Right
  {-# INLINE lift #-}
