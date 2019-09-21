{-# LANGUAGE DeriveFunctor, DeriveGeneric, KindSignatures #-}
module Control.Effect.Fail
( -- * Fail effect
  Fail(..)
, Fail.MonadFail(..)
) where

import Control.Carrier.Class
import qualified Control.Monad.Fail as Fail
import GHC.Generics (Generic1)

newtype Fail (m :: * -> *) k = Fail String
  deriving (Functor, Generic1)

instance HFunctor Fail
instance Effect   Fail
