{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Reader
( Reader(..)
, ask
, asks
, local
, runReader
, ReaderC(..)
) where

import Control.Effect.Handler
import Control.Effect.Sum
import Control.Effect.Internal

data Reader r m k
  = Ask (r -> k)
  | forall b . Local (r -> r) (m b) (b -> k)

deriving instance Functor (Reader r m)

instance HFunctor (Reader r) where
  hmap _ (Ask k)       = Ask k
  hmap f (Local g m k) = Local g (f m) k

instance Effect (Reader r) where
  handle state handler (Ask k)       = Ask (handler . (<$ state) . k)
  handle state handler (Local f m k) = Local f (handler (m <$ state)) (handler . fmap k)

-- | Retrieve the environment value.
--
--   prop> run (runReader a ask) == a
ask :: (Member (Reader r) sig, Carrier sig m) => m r
ask = send (Ask gen)

-- | Project a function out of the current environment value.
--
--   prop> snd (run (runReader a (asks (applyFun f)))) == applyFun f a
asks :: (Member (Reader r) sig, Carrier sig m, Functor m) => (r -> a) -> m a
asks f = fmap f ask

-- | Run a computation with an environment value locally modified by the passed function.
--
--   prop> run (runReader a (local (applyFun f) ask)) == applyFun f a
--   prop> run (runReader a ((,,) <$> ask <*> local (applyFun f) ask <*> ask)) == (a, applyFun f a, a)
local :: (Member (Reader r) sig, Carrier sig m) => (r -> r) -> m a -> m a
local f m = send (Local f m gen)


-- | Run a 'Reader' effect with the passed environment value.
--
--   prop> run (runReader a (pure b)) == b
runReader :: (Carrier sig m, Monad m) => r -> Eff (ReaderC r m) a -> m a
runReader r m = runReaderC (interpret m) r

newtype ReaderC r m a = ReaderC { runReaderC :: r -> m a }

instance (Carrier sig m, Monad m) => Carrier (Reader r :+: sig) (ReaderC r m) where
  gen a = ReaderC (const (gen a))
  alg = algR \/ algOther
    where algR (Ask       k) = ReaderC (\ r -> runReaderC (k r) r)
          algR (Local f m k) = ReaderC (\ r -> runReaderC m (f r) >>= flip runReaderC r . k)
          algOther op = ReaderC (\ r -> alg (handlePure (flip runReaderC r) op))


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
