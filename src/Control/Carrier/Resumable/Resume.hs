{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}

-- | Provides a carrier for 'Resumable' that can, given a handler function, resume the computation that threw an exception.
module Control.Carrier.Resumable.Resume
( -- * Resumable effect
  module Control.Effect.Resumable
  -- * Resumable carrier
, runResumable
, ResumableC(..)
  -- * Re-exports
, Carrier
, run
) where

import Control.Applicative (Alternative(..))
import Control.Carrier
import Control.Carrier.Reader
import Control.Effect.Resumable
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class

-- | Run a 'Resumable' effect, resuming uncaught errors with a given handler.
--
--   Note that this may be less efficient than defining a specialized carrier type and instance specifying the handler’s behaviour directly. Performance-critical code may wish to do that to maximize the opportunities for fusion and inlining.
--
--   >>> data Err a where Err :: Int -> Err Int
--
--   prop> run (runResumable (\ (Err b) -> pure (1 + b)) (pure a)) === a
--   prop> run (runResumable (\ (Err b) -> pure (1 + b)) (throwResumable (Err a))) === 1 + a
--
-- @since 1.0.0.0
runResumable
  :: (forall x . err x -> m x)
  -> ResumableC err m a
  -> m a
runResumable with = runReader (Handler with) . runResumableC

-- | @since 1.0.0.0
newtype ResumableC err m a = ResumableC { runResumableC :: ReaderC (Handler err m) m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans (ResumableC err) where
  lift = ResumableC . lift
  {-# INLINE lift #-}

instance MonadUnliftIO m => MonadUnliftIO (ResumableC err m) where
  askUnliftIO = ResumableC $ withUnliftIO $ \u -> return (UnliftIO (unliftIO u . runResumableC))
  {-# INLINE askUnliftIO #-}
  withRunInIO inner = ResumableC $ withRunInIO $ \run -> inner (run . runResumableC)
  {-# INLINE withRunInIO #-}

newtype Handler err m = Handler { runHandler :: forall x . err x -> m x }

instance Carrier sig m => Carrier (Resumable err :+: sig) (ResumableC err m) where
  eff (L (Resumable err k)) = ResumableC (ReaderC (\ handler -> runHandler handler err)) >>= k
  eff (R other)             = ResumableC (eff (R (handleCoercible other)))
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> :seti -XGADTs
-- >>> :seti -XTypeApplications
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
-- >>> import Data.Functor.Const
-- >>> import Data.Functor.Identity
