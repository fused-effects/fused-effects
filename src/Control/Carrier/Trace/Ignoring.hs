{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Trace.Ignoring
( -- * Trace effect
  Trace
, trace
  -- * Trace carrier
, runTrace
, TraceByIgnoringC(..)
-- * Re-exports
, Carrier
, Member
, run
) where

import Control.Applicative (Alternative(..))
import Control.Carrier.Class
import Control.Effect.Trace
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'Trace' effect, ignoring all traces.
--
--   prop> run (runTrace (trace a *> pure b)) === b
runTrace :: TraceByIgnoringC m a -> m a
runTrace = runTraceByIgnoringC

newtype TraceByIgnoringC m a = TraceByIgnoringC { runTraceByIgnoringC :: m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans TraceByIgnoringC where
  lift = TraceByIgnoringC
  {-# INLINE lift #-}

instance Carrier sig m => Carrier (Trace :+: sig) (TraceByIgnoringC m) where
  eff (L trace) = traceCont trace
  eff (R other) = TraceByIgnoringC (eff (handleCoercible other))
  {-# INLINE eff #-}
