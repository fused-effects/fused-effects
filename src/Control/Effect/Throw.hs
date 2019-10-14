{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts, KindSignatures #-}

{- | An effect for polymorphic failure.

Predefined carriers:

* "Control.Carrier.Throw.Either"
* "Control.Carrier.Error.Either" (with 'Control.Effect.Catch.Catch')
-}
module Control.Effect.Throw
( -- * Throw effect
  Throw(..)
, throwError
  -- * Properties
, throwError_annihilation
  -- * Re-exports
, Carrier
, Has
, run
) where

import Control.Carrier
import GHC.Generics (Generic1)

data Throw e (m :: * -> *) k
  = Throw e
  deriving (Functor, Generic1)

instance HFunctor (Throw e)
instance Effect   (Throw e)


-- | Throw an error, escaping the current computation up to the nearest 'Control.Effect.Catch.catchError' (if any).
--
-- @since 0.1.0.0
throwError :: Has (Throw e) sig m => e -> m a
throwError = send . Throw


-- Properties

-- | 'throwError' annihilates '>>='.
--
-- @since 1.0.0.0
throwError_annihilation :: Has (Throw e) sig m => (c -> c -> prop) -> (m b -> c) -> e -> (a -> m b) -> prop
throwError_annihilation (===) runThrow e k = runThrow (throwError e >>= k) === runThrow (throwError e)
