{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Reader
( Reader(..)
, ask
, asks
, local
, runReader
, ReaderC(..)
) where

import Control.Applicative (liftA2)
import Control.Effect.Carrier
import Control.Effect.Sum
import Control.Effect.Internal
import Control.Monad.Fail
import Control.Monad.IO.Class
import Prelude hiding (fail)

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
ask = send (Ask ret)

-- | Project a function out of the current environment value.
--
--   prop> snd (run (runReader a (asks (applyFun f)))) == applyFun f a
asks :: (Member (Reader r) sig, Carrier sig m) => (r -> a) -> m a
asks f = send (Ask (ret . f))

-- | Run a computation with an environment value locally modified by the passed function.
--
--   prop> run (runReader a (local (applyFun f) ask)) == applyFun f a
--   prop> run (runReader a ((,,) <$> ask <*> local (applyFun f) ask <*> ask)) == (a, applyFun f a, a)
local :: (Member (Reader r) sig, Carrier sig m) => (r -> r) -> m a -> m a
local f m = send (Local f m ret)


-- | Run a 'Reader' effect with the passed environment value.
--
--   prop> run (runReader a (pure b)) == b
runReader :: forall r sig m a . (Carrier sig m, Monad m) => r -> Eff (ReaderC r m) a -> m a
runReader r m = runReaderC (interpret m) r

newtype ReaderC r m a = ReaderC { runReaderC :: r -> m a }
  deriving (Functor)

instance Applicative m => Applicative (ReaderC r m) where
  pure a = ReaderC (const (pure a))
  ReaderC f <*> ReaderC a = ReaderC (liftA2 (<*>) f a)

instance Monad m => Monad (ReaderC r m) where
  ReaderC a >>= f = ReaderC (\ r -> a r >>= flip runReaderC r . f)

instance MonadFail m => MonadFail (ReaderC r m) where
  fail s = ReaderC (const (fail s))

instance MonadIO m => MonadIO (ReaderC r m) where
  liftIO = ReaderC . const . liftIO

instance (Carrier sig m, Monad m) => Carrier (Reader r :+: sig) (ReaderC r m) where
  ret = pure
  eff op = ReaderC (\ r -> handleSum (eff . handleReader r runReaderC) (\case
    Ask       k -> runReaderC (k r) r
    Local f m k -> runReaderC m (f r) >>= flip runReaderC r . k) op)


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
