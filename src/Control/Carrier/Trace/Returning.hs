{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Trace.Returning
( -- * Trace effect
  Trace
, trace
  -- * Trace carrier
, runTraceByReturning
, TraceC(..)
-- * Re-exports
, Carrier
, Member
, run
) where

import Control.Applicative (Alternative(..))
import Control.Carrier.Class
import Control.Carrier.State.Strict
import Control.Effect.Trace
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Bifunctor (first)

-- | Run a 'Trace' effect, returning all traces as a list.
--
--   prop> run (runTraceByReturning (trace a *> trace b *> pure c)) === ([a, b], c)
runTraceByReturning :: Functor m => TraceC m a -> m ([String], a)
runTraceByReturning = fmap (first reverse) . runState [] . runTraceC

newtype TraceC m a = TraceC { runTraceC :: StateC [String] m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Carrier sig m, Effect sig) => Carrier (Trace :+: sig) (TraceC m) where
  eff (L (Trace m k)) = TraceC (modify (m :)) *> k
  eff (R other)       = TraceC (eff (R (handleCoercible other)))
