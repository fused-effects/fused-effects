{-# LANGUAGE DeriveFunctor, MultiParamTypeClasses #-}
module Control.Carrier.Maybe where

import Control.Applicative (liftA2)
import Control.Carrier

newtype MaybeH m a = MaybeH { runMaybeH :: m (Maybe a) }
  deriving (Functor)

instance Applicative m => Applicative (MaybeH m) where
  pure a = MaybeH (pure (Just a))

  MaybeH f <*> MaybeH a = MaybeH (liftA2 (<*>) f a)

instance Monad m => Monad (MaybeH m) where
  return = pure

  MaybeH a >>= f = MaybeH (a >>= maybe (pure Nothing) (runMaybeH . f))

instance Carrier Maybe MaybeH where
  joinl mf = MaybeH (mf >>= runMaybeH)

  suspend = MaybeH (pure (Just (Just ())))

  resume = maybe (pure Nothing) runMaybeH

  wrap = MaybeH
