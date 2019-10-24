{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
module Control.Effect.Lift.Internal
( Lift(..)
) where

import Control.Effect.Class
import GHC.Generics (Generic1)

-- | @since 0.1.0.0
newtype Lift sig m k = Lift { unLift :: sig (m k) }
  deriving (Functor, Generic1)

instance (Functor f, Functor m) => Effect f (Lift m)
