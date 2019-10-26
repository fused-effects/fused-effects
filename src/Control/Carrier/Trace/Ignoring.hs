{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

-- | A carrier for the 'Control.Effect.Trace' effect that ignores all traced results. Useful when you wish to disable tracing without removing all trace statements.
--
-- @since 1.0.0.0
module Control.Carrier.Trace.Ignoring
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

-- | Run a 'Trace' effect, ignoring all traces.
--
-- @
-- 'runTrace' ('trace' s) = 'pure' ()
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

instance MonadTransContext TraceC where
  liftHandle handle = TraceC (liftIdentity handle runTrace)
  {-# INLINE liftHandle #-}

instance MonadUnliftIO m => MonadUnliftIO (TraceC m) where
  askUnliftIO = TraceC $ withUnliftIO $ \u -> return (UnliftIO (unliftIO u . runTrace))
  {-# INLINE askUnliftIO #-}
  withRunInIO inner = TraceC $ withRunInIO $ \run -> inner (run . runTrace)
  {-# INLINE withRunInIO #-}

instance AlgebraTrans Trace TraceC where
  liftAlg = traceCont
  {-# INLINE liftAlg #-}
