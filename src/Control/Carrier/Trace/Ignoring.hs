{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

-- | A carrier for the 'Control.Effect.Trace' effect that ignores all traced results. Useful when you wish to disable tracing without removing all trace statements.
module Control.Carrier.Trace.Ignoring
( -- * Trace carrier
  runTrace
, TraceC(..)
  -- * Trace effect
, module Control.Effect.Trace
) where

import Control.Applicative (Alternative(..))
import Control.Carrier
import Control.Effect.Trace
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
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
runTrace = runTraceC

-- | @since 1.0.0.0
newtype TraceC m a = TraceC { runTraceC :: m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans TraceC where
  lift = TraceC
  {-# INLINE lift #-}

instance Carrier sig m => Carrier (Trace :+: sig) (TraceC m) where
  eff (L trace) = traceCont trace
  eff (R other) = TraceC (eff (handleCoercible other))
  {-# INLINE eff #-}
