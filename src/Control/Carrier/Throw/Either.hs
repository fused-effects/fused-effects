{-# LANGUAGE DeriveFunctor #-}
module Control.Carrier.Throw.Either
( -- * Throw effect
  module Control.Effect.Throw
  -- * Throw carrier
, runThrow
, ThrowC(..)
  -- * Re-exports
, Carrier
, run
) where

import Control.Carrier
import Control.Carrier.Error.Either
import Control.Effect.Throw

runThrow :: ThrowC e m a -> m (Either e a)
runThrow = runError . runThrowC

newtype ThrowC e m a = ThrowC { runThrowC :: ErrorC e m a }
  deriving (Functor)
