{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts, FlexibleInstances, KindSignatures, MultiParamTypeClasses #-}

{- | An effect for polymorphic failure.

Predefined carriers:

* "Control.Carrier.Throw.Either"
* "Control.Carrier.Error.Either" (with 'Control.Effect.Catch.Catch')

@since 1.0.0.0
-}
module Control.Effect.Throw
( -- * Throw effect
  Throw(..)
, throwError
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import GHC.Generics (Generic1)

-- | @since 1.0.0.0
data Throw e (m :: * -> *) k
  = Throw e
  deriving (Functor, Generic1)

instance Functor f => Effect f (Throw e)


-- | Throw an error, escaping the current computation up to the nearest 'Control.Effect.Catch.catchError' (if any).
--
-- @since 0.1.0.0
throwError :: Has (Throw e) sig m => e -> m a
throwError = send . Throw
