{-# LANGUAGE DeriveFunctor, DeriveGeneric, KindSignatures #-}
module Control.Effect.Throw
( -- * Throw effect
  Throw(..)
) where

import Control.Carrier
import GHC.Generics (Generic1)

data Throw e (m :: * -> *) k
  = Throw e
  deriving (Functor, Generic1)

instance HFunctor (Throw e)
