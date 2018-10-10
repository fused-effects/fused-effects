{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Reader where

import Control.Effect
import Data.Functor.Identity

data Reader r m k
  = Ask (r -> k)
  | forall b . Local (r -> r) (m b) (b -> k)

deriving instance Functor (Reader r m)

instance Effect (Reader r) where
  hfmap _ (Ask k)       = Ask k
  hfmap f (Local g m k) = Local g (f m) k

  handle state handler (Ask k)       = Ask (handler . (<$ state) . k)
  handle state handler (Local f m k) = Local f (handler (m <$ state)) (handler . fmap k)

ask :: (Subset (Reader r) sig, TermMonad m sig) => m r
ask = send (Ask pure)

local :: (Subset (Reader r) sig, TermMonad m sig) => (r -> r) -> m a -> m a
local f m = send (Local f m pure)


runReader :: TermMonad m sig => r -> Eff (ReaderH r m) a -> m a
runReader r m = runReaderH (interpret m) r


newtype ReaderH r m a = ReaderH { runReaderH :: r -> m a }

instance TermMonad m sig => TermAlgebra (ReaderH r m) (Reader r :+: sig) where
  var a = ReaderH (\ _ -> pure a)
  con = alg \/ algOther
    where alg (Ask       k) = ReaderH (\ r -> runReaderH (k r) r)
          alg (Local f m k) = ReaderH (\ r -> runReaderH m (f r) >>= flip runReaderH r . k)
          algOther op = ReaderH (\ r -> runIdentity <$> con (handle (Identity ()) (fmap Identity . flip runReaderH r . runIdentity) op))
