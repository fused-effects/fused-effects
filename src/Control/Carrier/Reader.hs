{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
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

import           Control.Algebra
import           Control.Applicative (Alternative)
import           Control.Effect.Reader
import           Control.Monad (MonadPlus)
import qualified Control.Monad.Fail as Fail
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Reader as R

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
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus) via R.ReaderT r m

instance MonadTrans (ReaderC r) where
  lift = ReaderC . const
  {-# INLINE lift #-}

instance Algebra sig m => Algebra (Reader r :+: sig) (ReaderC r m) where
  alg (L (Ask       k)) = ReaderC (\ r -> runReader r (k r))
  alg (L (Local f m k)) = ReaderC (\ r -> runReader (f r) m) >>= k
  alg (R other)         = ReaderC (\ r -> alg (hmap (runReader r) other))
  {-# INLINE alg #-}
