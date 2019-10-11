{-# LANGUAGE DeriveFunctor, DeriveGeneric, KindSignatures #-}

{- | An effect providing failure with an error message.

This effect is invoked through the 'Fail.fail' method from 'Fail.MonadFail'.

Predefined carriers:

* "Control.Carrier.Fail.Either"
-}

module Control.Effect.Fail
( -- * Fail effect
  Fail(..)
, Fail.MonadFail(..)
  -- * Re-exports
, Has
) where

import Control.Carrier
import qualified Control.Monad.Fail as Fail
import GHC.Generics (Generic1)

-- | @since 1.0.0.0
newtype Fail (m :: * -> *) k = Fail String
  deriving (Functor, Generic1)

instance HFunctor Fail
instance Effect   Fail
