{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

-- | A carrier for the 'Control.Effect.Trace' effect that aggregates and returns all traced values.
--
-- @since 1.0.0.0
module Control.Carrier.Trace.Returning
( -- * Trace carrier
  runTrace
, TraceC(..)
  -- * Trace effect
, module Control.Effect.Trace
) where

import Control.Algebra
import Control.Applicative (Alternative(..))
import Control.Carrier.Writer.Strict
import Control.Effect.Trace
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Bifunctor (first)
import Data.Monoid (Endo(..))

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
runTrace (TraceC m) = first (($[]) . appEndo) <$> runWriter m

-- | @since 1.0.0.0
newtype TraceC m a = TraceC (WriterC (Endo [String]) m a)
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Algebra sig m, Effect sig) => Algebra (Trace :+: sig) (TraceC m) where
  alg (L (Trace m k)) = TraceC (tell (Endo (m :))) *> k
  alg (R other)       = TraceC (alg (R (handleCoercible other)))
