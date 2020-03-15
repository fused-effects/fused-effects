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
    a (\ a' -> b (\ b' -> k (f a' b')))
  {-# INLINE liftA2 #-}

  ErrorC a *> ErrorC b = ErrorC $ a . const . b
  {-# INLINE (*>) #-}

  ErrorC a <* ErrorC b = ErrorC $ \ k -> a (b . const . k)
  {-# INLINE (<*) #-}
