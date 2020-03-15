{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
module Control.Carrier.Error.Cont
( -- * Error carrier
  runError
, ErrorC(..)
  -- * Error effect
, module Control.Effect.Error
) where

import Control.Applicative (liftA2)
import Control.Effect.Error
import Control.Monad.Fail as Fail
import Control.Monad.Trans.Class

runError :: (a -> m (Either e b)) -> ErrorC e m a -> m (Either e b)
runError f (ErrorC m) = m f
{-# INLINE runError #-}

newtype ErrorC e m a = ErrorC (forall b . (a -> m (Either e b)) -> m (Either e b))
  deriving (Functor)

instance Applicative (ErrorC e m) where
  pure a = ErrorC $ \ k -> k a
  {-# INLINE pure #-}

  ErrorC f <*> ErrorC a = ErrorC $ \ k ->
    f (\ f' -> a (k . f'))
  {-# INLINE (<*>) #-}

  liftA2 f (ErrorC a) (ErrorC b) = ErrorC $ \ k ->
    a (\ a' -> b (k . f a'))
  {-# INLINE liftA2 #-}

  ErrorC a *> ErrorC b = ErrorC $ a . const . b
  {-# INLINE (*>) #-}

  ErrorC a <* ErrorC b = ErrorC $ \ k -> a (b . const . k)
  {-# INLINE (<*) #-}

instance Monad (ErrorC e m) where
  ErrorC a >>= f = ErrorC $ \ k -> a (runError k . f)
  {-# INLINE (>>=) #-}

instance Fail.MonadFail m => Fail.MonadFail (ErrorC e m) where
  fail = lift . Fail.fail
  {-# INLINE fail #-}

instance MonadTrans (ErrorC e) where
  lift m = ErrorC $ \ k -> m >>= k
  {-# INLINE lift #-}
