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

instance Applicative (ErrorC e m) where
  pure a = ErrorC $ \ k -> k (Right a)
  {-# INLINE pure #-}

  ErrorC f <*> ErrorC a = ErrorC $ \ k ->
    f (either (k . Left) (\ f' -> a (either (k . Left) (k . Right . f'))))
  {-# INLINE (<*>) #-}

  ErrorC a *> ErrorC b = ErrorC $ \ k ->
    a (either (k . Left) (const (b k)))
  {-# INLINE (*>) #-}

  ErrorC a <* ErrorC b = ErrorC $ \ k ->
    a (either (k . Left) (b . const . k . Right))
  {-# INLINE (<*) #-}
