module Control.Carrier.Throw.Error
( -- * Throw effect
  module Control.Effect.Throw
  -- * Throw carrier
, runThrow
, ThrowC(ThrowC)
  -- * Re-exports
, Carrier
, run
) where

import Control.Carrier
import Control.Effect.Throw

newtype ThrowC e m a = ThrowC { runThrow :: m a }
