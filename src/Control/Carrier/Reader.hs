{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

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

import Control.Applicative (Alternative(..), liftA2)
import Control.Carrier.Class
import Control.Effect.Reader
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class

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

instance MonadUnliftIO m => MonadUnliftIO (ReaderC r m) where
  askUnliftIO = ReaderC $ \r -> withUnliftIO $ \u -> pure (UnliftIO (\(ReaderC x) -> unliftIO u (x r)))
  {-# INLINE askUnliftIO #-}
  withRunInIO inner = ReaderC $ \r -> withRunInIO $ \go -> inner (go . runReader r)
  {-# INLINE withRunInIO #-}

instance Carrier sig m => Carrier (Reader r :+: sig) (ReaderC r m) where
  eff (L (Ask       k)) = ReaderC (\ r -> runReader r (k r))
  eff (L (Local f m k)) = ReaderC (\ r -> runReader (f r) m) >>= k
  eff (R other)         = ReaderC (\ r -> eff (hmap (runReader r) other))
  {-# INLINE eff #-}
