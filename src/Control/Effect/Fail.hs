{-# LANGUAGE DeriveFunctor, DeriveGeneric, KindSignatures #-}

{- | An effect providing failure with an error message.

This effect is invoked through the 'Fail.fail' method from 'Fail.MonadFail'.

Predefined carriers:

* "Control.Carrier.Fail.Either"
-}

module Control.Effect.Fail
( -- * Fail effect
  Fail
, Fail.MonadFail(..)
  -- * Re-exports
, Carrier
, Has
, run
) where

import Control.Effect.Throw
import qualified Control.Monad.Fail as Fail

-- | @since 1.0.0.0
type Fail = Throw String
