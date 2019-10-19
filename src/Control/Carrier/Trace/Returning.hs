{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

-- | A carrier for the 'Control.Effect.Trace' effect that aggregates and returns all traced values.
module Control.Carrier.Trace.Returning
( -- * Trace carrier
  runTrace
, TraceC(..)
  -- * Trace effect
, module X
) where

import Control.Applicative (Alternative(..))
import Control.Carrier
import Control.Carrier.State.Strict
import Control.Effect.Trace
import Control.Effect.Trace as X (Trace)
import Control.Effect.Trace as X hiding (Trace)
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Bifunctor (first)

-- | Run a 'Trace' effect, returning all traces as a list.
--
-- @
-- 'runTrace' ('pure' a) = 'pure' ([], a)
-- @
-- @
-- 'runTrace' ('trace' s) = 'pure' ([s], ())
-- @
--
-- @since 1.0.0.0
runTrace :: Functor m => TraceC m a -> m ([String], a)
runTrace = fmap (first reverse) . runState [] . runTraceC

-- | @since 1.0.0.0
newtype TraceC m a = TraceC { runTraceC :: StateC [String] m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Carrier sig m, Effect sig) => Carrier (Trace :+: sig) (TraceC m) where
  eff (L (Trace m k)) = TraceC (modify (m :)) *> k
  eff (R other)       = TraceC (eff (R (handleCoercible other)))
