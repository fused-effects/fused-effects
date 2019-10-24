{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
module Control.Effect.Lift.Internal
( Lift(..)
) where

import Control.Effect.Class
import GHC.Generics (Generic1)

-- | @since 0.1.0.0
newtype Lift sig m k = Lift { unLift :: sig (m k) }
  deriving (Functor, Generic1)

instance Functor m => Effect (Lift m)
