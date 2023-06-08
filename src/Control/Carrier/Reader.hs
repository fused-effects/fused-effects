{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A carrier for 'Reader' effects.
--
-- @since 1.0.0.0
module Control.Carrier.Reader
( -- * Reader carrier
  runReader
, ReaderC(..)
  -- * Reader effect
, module Control.Effect.Reader
) where

import Control.Algebra
import Control.Applicative
import Control.Effect.Reader
import Control.Monad (MonadPlus)
import Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.IO.Unlift

-- | Run a 'Reader' effect with the passed environment value.
--
-- @
-- 'runReader' a 'ask' = 'pure' a
-- @
-- @
-- 'runReader' a ('pure' b) = 'pure' b
-- @
-- @
-- 'runReader' a ('local' f m) = 'runReader' (f a) m
-- @
--
-- @since 1.0.0.0
runReader :: r -> ReaderC r m a -> m a
runReader r (ReaderC runReaderC) = runReaderC r
{-# INLINE runReader #-}

-- | @since 1.0.0.0
newtype ReaderC r m a = ReaderC (r -> m a)
  deriving (Functor)

instance Applicative m => Applicative (ReaderC r m) where
  pure = ReaderC . const . pure
  {-# INLINE pure #-}

  ReaderC f <*> ReaderC a = ReaderC (liftA2 (<*>) f a)
  {-# INLINE (<*>) #-}

  liftA2 f (ReaderC a) (ReaderC b) = ReaderC $ \ r ->
    liftA2 f (a r) (b r)
  {-# INLINE liftA2 #-}

  ReaderC u *> ReaderC v = ReaderC $ \ r -> u r *> v r
  {-# INLINE (*>) #-}

  ReaderC u <* ReaderC v = ReaderC $ \ r -> u r <* v r
  {-# INLINE (<*) #-}

instance Alternative m => Alternative (ReaderC r m) where
  empty = ReaderC (const empty)
  {-# INLINE empty #-}

  ReaderC l <|> ReaderC r = ReaderC (liftA2 (<|>) l r)
  {-# INLINE (<|>) #-}

instance Monad m => Monad (ReaderC r m) where
  ReaderC a >>= f = ReaderC (\ r -> a r >>= runReader r . f)
  {-# INLINE (>>=) #-}

instance Fail.MonadFail m => Fail.MonadFail (ReaderC r m) where
  fail = ReaderC . const . Fail.fail
  {-# INLINE fail #-}

instance MonadFix m => MonadFix (ReaderC s m) where
  mfix f = ReaderC (\ r -> mfix (runReader r . f))
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (ReaderC r m) where
  liftIO = ReaderC . const . liftIO
  {-# INLINE liftIO #-}

instance (Alternative m, Monad m) => MonadPlus (ReaderC r m)

instance MonadTrans (ReaderC r) where
  lift = ReaderC . const
  {-# INLINE lift #-}

instance Algebra sig m => Algebra (Reader r :+: sig) (ReaderC r m) where
  alg hdl sig ctx = ReaderC $ \ r -> case sig of
    L Ask         -> pure (r <$ ctx)
    L (Local f m) -> runReader (f r) (hdl (m <$ ctx))
    R other       -> alg (runReader r . hdl) other ctx
  {-# INLINE alg #-}

instance MonadUnliftIO m => MonadUnliftIO (ReaderC r m) where
  withRunInIO inner = ReaderC $ \ r -> withRunInIO $ \ run -> inner (run . runReader r)
  {-# INLINE withRunInIO #-}
