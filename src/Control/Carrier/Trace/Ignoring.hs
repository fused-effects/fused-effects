{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Trace.Ignoring
( -- * Trace effect
  Trace
, trace
  -- * Trace carrier
, runTrace
, TraceC(..)
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
runTrace :: TraceC m a -> m a
runTrace = runTraceC

newtype TraceC m a = TraceC { runTraceC :: m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans TraceC where
  lift = TraceC
  {-# INLINE lift #-}

instance Carrier sig m => Carrier (Trace :+: sig) (TraceC m) where
  eff (L trace) = traceCont trace
  eff (R other) = TraceC (eff (handleCoercible other))
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
