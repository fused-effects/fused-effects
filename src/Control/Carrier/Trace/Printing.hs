{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

-- | A carrier for the 'Control.Effect.Trace' effect that prints all traced results to stderr.
--
-- @since 1.0.0.0
module Control.Carrier.Trace.Printing
( -- * Trace carrier
  runTrace
, TraceC(..)
  -- * Trace effect
, module Control.Effect.Trace
) where

import Control.Algebra
import Control.Applicative (Alternative(..))
import Control.Effect.Trace
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class
import System.IO

-- | Run a 'Trace' effect, printing traces to 'stderr'.
--
-- @
-- 'runTrace' ('trace' s) = 'liftIO' ('putStrLn' s)
-- @
-- @
-- 'runTrace' ('pure' a) = 'pure' a
-- @
--
-- @since 1.0.0.0
runTrace :: TraceC m a -> m a
runTrace (TraceC m) = m

-- | @since 1.0.0.0
newtype TraceC m a = TraceC (m a)
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans TraceC where
  lift = TraceC
  {-# INLINE lift #-}

instance MonadUnliftIO m => MonadUnliftIO (TraceC m) where
  askUnliftIO = TraceC $ withUnliftIO $ \u -> return (UnliftIO (unliftIO u . runTrace))
  {-# INLINE askUnliftIO #-}
  withRunInIO inner = TraceC $ withRunInIO $ \run -> inner (run . runTrace)
  {-# INLINE withRunInIO #-}

instance (MonadIO m, Algebra sig m) => Algebra (Trace :+: sig) (TraceC m) where
  eff (L (Trace s k)) = liftIO (hPutStrLn stderr s) *> k
  eff (R other)       = TraceC (eff (handleCoercible other))
  {-# INLINE eff #-}
