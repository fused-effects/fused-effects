module Control.Carrier.Throw.Either
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

newtype ThrowC e m a = ThrowC { runThrow :: m (Either e a) }
