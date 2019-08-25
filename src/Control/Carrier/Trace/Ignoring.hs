module Control.Carrier.Trace.Ignoring
( -- * Trace effect
  Trace
, trace
  -- * Trace carrier
, runTraceByIgnoring
, TraceByIgnoringC(..)
-- * Re-exports
, Carrier
, Member
, run
) where

import Control.Effect.Trace
