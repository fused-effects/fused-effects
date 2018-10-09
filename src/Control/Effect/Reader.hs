{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Reader where

import Control.Effect
import Control.Monad.Codensity

data Reader r m k
  = Ask (r -> k)
  | forall b . Local (r -> r) (m b) (b -> k)

deriving instance Functor (Reader r m)

instance Effect (Reader r) where
  hfmap _ (Ask k)       = Ask k
  hfmap f (Local g m k) = Local g (f m) k

  handle _     (Ask k)       = Ask k
  handle state (Local f m k) = Local f (resume (m <$ state)) (wrap . resume . fmap k)

ask :: (Subset (Reader r) sig, TermMonad m sig) => m r
ask = send (Ask pure)

local :: (Subset (Reader r) sig, TermMonad m sig) => (r -> r) -> m a -> m a
local f m = send (Local f m pure)


runReader :: TermMonad m sig => r -> Codensity (ReaderH r m) a -> m a
runReader r m = runReaderH (runCodensity var m) r


newtype ReaderH r m a = ReaderH { runReaderH :: r -> m a }
  deriving (Functor)

instance Carrier ((,) r) (ReaderH r) where
  joinl mf = ReaderH (\ r -> mf >>= \ f -> runReaderH f r)

  suspend f = ReaderH (\ r -> runReaderH (f (r, ())) r)

  resume (r, m) = (,) r <$> runReaderH m r

  wrap = ReaderH . const . fmap snd

  gen a = ReaderH (\ _ -> pure a)

instance TermMonad m sig => TermAlgebra (ReaderH r m) (Reader r :+: sig) where
  var = gen
  con = alg \/ interpretRest
    where alg (Ask       k) = ReaderH (\ r -> runReaderH (k r) r)
          alg (Local f m k) = ReaderH (\ r -> runReaderH m (f r) >>= flip runReaderH r . k)
