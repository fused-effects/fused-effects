{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts #-}
module Control.Effect.Trace
( -- * Trace effect
  Trace(..)
, trace
  -- * Re-exports
, Has
) where

import Control.Carrier
import GHC.Generics (Generic1)

data Trace m k = Trace
  { traceMessage :: String
  , traceCont    :: m k
  }
  deriving (Functor, Generic1)

instance HFunctor Trace
instance Effect   Trace

-- | Append a message to the trace log.
trace :: Has Trace sig m => String -> m ()
trace message = send (Trace message (pure ()))
