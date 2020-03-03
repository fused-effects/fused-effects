{-# LANGUAGE DeriveFunctor #-}

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

-- | @since 0.1.0.0
data Trace m k = Trace
  { traceMessage :: String
  , traceCont    :: m k
  }
  deriving (Functor)

-- | Append a message to the trace log.
--
-- @since 0.1.0.0
trace :: Has Trace sig m => String -> m ()
trace message = send (Trace message (pure ()))
