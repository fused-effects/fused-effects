{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{- | An effect that provides a record of 'String' values ("traces") aggregate during the execution of a given computation.

Predefined carriers:

* "Control.Carrier.Trace.Printing", which logs to stderr in a 'Control.Monad.IO.Class.MonadIO' context.
* "Control.Carrier.Trace.Returning", which aggregates all traces in a @[String].
* "Control.Carrier.Trace.Ignoring", which discards all traced values.

@since 0.1.0.0
-}

module Control.Effect.Trace
( -- * Trace effect
  Trace(..)
, trace
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import Data.Kind (Type)

-- | @since 0.1.0.0
data Trace (m :: Type -> Type) k where
  Trace :: { traceMessage :: String } -> Trace m ()

-- | Append a message to the trace log.
--
-- @since 0.1.0.0
trace :: Has Trace sig m => String -> m ()
trace message = send (Trace message)
{-# INLINE trace #-}
