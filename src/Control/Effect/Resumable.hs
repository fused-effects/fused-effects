{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, StandaloneDeriving #-}
-- | An effect providing the ability to throw exceptions from a context. If an exception is
-- thrown, the calling context may choose to resume the computation. Type safety of the
-- resumed operation is preserved by parametricity achieved from the @-XGADTs@ extension.
--
-- Predefined carriers:
--
-- * "Control.Carrier.Resumable.Resume", which provides full resumption semantics.
-- * "Control.Carrier.Resumable.Either", which elides resumption support (like @Control.Effect.Error@).
module Control.Effect.Resumable
( -- * Resumable effect
  Resumable(..)
, throwResumable
  -- * Re-exports
, Carrier
, Has
, run
) where

import Control.Carrier

-- | Errors which can be resumed with values of some existentially-quantified type.
--
-- @since 0.1.0.0
data Resumable err m k
  = forall a . Resumable (err a) (a -> m k)

deriving instance Functor m => Functor (Resumable err m)

instance HFunctor (Resumable err) where
  hmap f (Resumable err k) = Resumable err (f . k)

instance Effect (Resumable err) where
  handle state handler (Resumable err k) = Resumable err (handler . (<$ state) . k)

-- | Throw an error which can be resumed with a value of its result type. Note that the type parameters in the @err a@ paramater and @m a@ parameter must match up; this indicates the type with which the error must be resumed.
--
-- @since 0.1.0.0
throwResumable :: Has (Resumable err) sig m => err a -> m a
throwResumable err = send (Resumable err pure)
