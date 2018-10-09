{-# LANGUAGE DeriveFunctor, MultiParamTypeClasses #-}
module Control.Carrier.Reader where

import Control.Carrier

newtype ReaderH r m a = ReaderH { runReaderH :: r -> m a }
  deriving (Functor)

instance Applicative m => Applicative (ReaderH r m) where
  pure a = ReaderH (\ _ -> pure a)

  ReaderH f <*> ReaderH a = ReaderH (\ r -> f r <*> a r)

instance Monad m => Monad (ReaderH r m) where
  return = pure

  ReaderH a >>= f = ReaderH (\ r -> a r >>= \ a' -> runReaderH (f a') r)

instance Carrier ((,) r) (ReaderH r) where
  joinl mf = ReaderH (\ r -> mf >>= \ f -> runReaderH f r)

  suspend = ReaderH (\ r -> pure (r, ()))

  resume (r, m) = (,) r <$> runReaderH m r

  wrap = ReaderH . const . fmap snd
