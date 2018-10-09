{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, StandaloneDeriving, TypeOperators #-}
module Control.Effect.Reader where

import Control.Carrier.Reader
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


runReader :: TermMonad m sig => r -> Eff (Reader r :+: sig) a -> m a
runReader r m = runReaderH (interpret alg m) r
  where alg (Ask k)       = ReaderH (\ r -> runReaderH (k r) r)
        alg (Local f m k) = ReaderH (\ r -> runReaderH m (f r) >>= flip runReaderH r . k)
