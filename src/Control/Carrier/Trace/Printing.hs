{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Trace.Printing
( -- * Trace effect
  Trace
, trace
  -- * Trace carrier
, runTraceByPrinting
, TraceByPrintingC(..)
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
import System.IO

-- | Run a 'Trace' effect, printing traces to 'stderr'.
runTraceByPrinting :: TraceByPrintingC m a -> m a
runTraceByPrinting = runTraceByPrintingC

newtype TraceByPrintingC m a = TraceByPrintingC { runTraceByPrintingC :: m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans TraceByPrintingC where
  lift = TraceByPrintingC
  {-# INLINE lift #-}

instance (MonadIO m, Carrier sig m) => Carrier (Trace :+: sig) (TraceByPrintingC m) where
  eff (L (Trace s k)) = liftIO (hPutStrLn stderr s) *> k
  eff (R other)       = TraceByPrintingC (eff (handleCoercible other))
  {-# INLINE eff #-}
