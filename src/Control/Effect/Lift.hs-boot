module Control.Effect.Lift
( Lift(..)
) where

import Control.Effect.Class

newtype Lift sig m k = Lift { unLift :: sig (m k) }

instance Functor m => HFunctor (Lift m)
instance Functor m => Effect   (Lift m)
