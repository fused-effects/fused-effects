{-# LANGUAGE DeriveFunctor, DeriveGeneric, KindSignatures #-}
module Control.Effect.Empty
( -- * Empty effect
  Empty(..)
) where

import Control.Carrier.Class
import GHC.Generics (Generic1)

data Empty (m :: * -> *) k = Empty
  deriving (Functor, Generic1)

instance HFunctor Empty
instance Effect Empty
