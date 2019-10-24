{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
module Control.Effect.Choose.Internal
( Choose(..)
) where

import Control.Effect.Class
import GHC.Generics (Generic1)

-- | @since 1.0.0.0
newtype Choose m k
  = Choose (Bool -> m k)
  deriving (Functor, Generic1)

instance Effect Choose
