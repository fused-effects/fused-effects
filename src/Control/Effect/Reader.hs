{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Reader where

import Control.Effect

data Reader r m k
  = Ask (r -> k)
  | forall b . Local (r -> r) (m b) (b -> k)

deriving instance Functor (Reader r m)

instance HFunctor (Reader r) where
  hfmap _ (Ask k)       = Ask k
  hfmap f (Local g m k) = Local g (f m) k

instance Effect (Reader r) where
  handle state handler (Ask k)       = Ask (handler . (<$ state) . k)
  handle state handler (Local f m k) = Local f (handler (m <$ state)) (handler . fmap k)

ask :: (Subset (Reader r) sig, Effectful sig m) => m r
ask = send (Ask pure)

local :: (Subset (Reader r) sig, Effectful sig m) => (r -> r) -> m a -> m a
local f m = send (Local f m pure)


runReader :: (Carrier sig m, Monad m) => r -> Eff (ReaderH r m) a -> m a
runReader r m = runReaderH (interpret m) r


newtype ReaderH r m a = ReaderH { runReaderH :: r -> m a }

instance (Carrier sig m, Monad m) => Carrier (Reader r :+: sig) (ReaderH r m) where
  gen a = ReaderH (\ _ -> pure a)
  alg = algR \/ algOther
    where algR (Ask       k) = ReaderH (\ r -> runReaderH (k r) r)
          algR (Local f m k) = ReaderH (\ r -> runReaderH m (f r) >>= flip runReaderH r . k)
          algOther op = ReaderH (\ r -> alg (handlePure (flip runReaderH r) op))
