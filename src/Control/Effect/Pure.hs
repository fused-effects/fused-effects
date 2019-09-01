{-# LANGUAGE DeriveFunctor, DeriveGeneric, KindSignatures #-}
module Control.Effect.Pure
( -- * Pure effect
  Pure
) where

import Control.Effect.Class
import GHC.Generics (Generic1)

data Pure (m :: * -> *) k
  deriving (Functor, Generic1)

instance HFunctor Pure
instance Effect   Pure
