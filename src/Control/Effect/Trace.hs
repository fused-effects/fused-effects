{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric, DerivingStrategies, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Effect.Trace
( -- * Trace effect
  Trace(..)
, trace
  -- * Trace carriers
, runTraceByReturning
, TraceByReturningC(..)
  -- * Re-exports
, Carrier
, Member
, run
) where

import Control.Applicative (Alternative(..))
import Control.Carrier.Class
import Control.Carrier.State.Strict
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Bifunctor (first)
import GHC.Generics (Generic1)

data Trace m k = Trace
  { traceMessage :: String
  , traceCont    :: m k
  }
  deriving stock (Functor, Generic1)
  deriving anyclass (HFunctor, Effect)

-- | Append a message to the trace log.
trace :: (Member Trace sig, Carrier sig m) => String -> m ()
trace message = send (Trace message (pure ()))


-- | Run a 'Trace' effect, returning all traces as a list.
--
--   prop> run (runTraceByReturning (trace a *> trace b *> pure c)) === ([a, b], c)
runTraceByReturning :: Functor m => TraceByReturningC m a -> m ([String], a)
runTraceByReturning = fmap (first reverse) . runState [] . runTraceByReturningC

newtype TraceByReturningC m a = TraceByReturningC { runTraceByReturningC :: StateC [String] m a }
  deriving newtype (Alternative, Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Carrier sig m, Effect sig) => Carrier (Trace :+: sig) (TraceByReturningC m) where
  eff (L (Trace m k)) = TraceByReturningC (modify (m :)) *> k
  eff (R other)       = TraceByReturningC (eff (R (handleCoercible other)))


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
