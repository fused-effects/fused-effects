{-# LANGUAGE DeriveFunctor, PolyKinds #-}
module Control.Effect.Lift.Internal
( Lift(..)
) where

import Control.Effect.Carrier
import Data.Coerce

newtype Lift sig m k = Lift { unLift :: sig k }
  deriving (Functor)

instance Functor sig => HFunctor (Lift sig) where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Functor sig => Effect (Lift sig) where
  handle state handler (Lift op) = Lift (fmap (handler . (<$ state)) op)
