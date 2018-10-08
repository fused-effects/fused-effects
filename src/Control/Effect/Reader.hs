{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, StandaloneDeriving, TypeOperators, MultiParamTypeClasses #-}
module Control.Effect.Reader where

import Control.Effect

data Reader r m k
  = Ask (r -> k)
  | forall b . Local (r -> r) (m b) (b -> k)

deriving instance Functor (Reader r m)

instance Effect (Reader r) where
  hfmap _ (Ask k)       = Ask k
  hfmap f (Local g m k) = Local g (f m) k

  handle _     (Ask k)       = Ask k
  handle state (Local f m k) = Local f (resume (m <$ state)) (wrap . resume . fmap k)

ask :: Subset (Reader r) sig => Eff sig r
ask = send (Ask pure)

local :: Subset (Reader r) sig => (r -> r) -> Eff sig a -> Eff sig a
local f m = send (Local f m pure)


runReader :: Effect sig => r -> Eff (Reader r :+: sig) a -> Eff sig a
runReader r m = runReaderH (relay alg m) r
  where alg (Ask k)       = ReaderH (\ r -> runReaderH (k r) r)
        alg (Local f m k) = ReaderH (\ r -> runReaderH m (f r) >>= flip runReaderH r . k)


newtype ReaderH r m a = ReaderH { runReaderH :: r -> m a }
  deriving (Functor)

instance Applicative m => Applicative (ReaderH r m) where
  pure a = ReaderH (\ _ -> pure a)

  ReaderH f <*> ReaderH a = ReaderH (\ r -> f r <*> a r)

instance Monad m => Monad (ReaderH r m) where
  return = pure

  ReaderH a >>= f = ReaderH (\ r -> a r >>= \ a' -> runReaderH (f a') r)

instance Carrier (ReaderH r) ((,) r) where
  joinl mf = ReaderH (\ r -> mf >>= \ f -> runReaderH f r)

  suspend = ReaderH (\ r -> pure (r, ()))

  resume (r, m) = (,) r <$> runReaderH m r

  wrap = ReaderH . const . fmap snd
