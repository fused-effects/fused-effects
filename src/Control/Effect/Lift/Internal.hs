{-# LANGUAGE DeriveFunctor, PolyKinds #-}
module Control.Effect.Lift.Internal where

import Control.Effect.Handler

newtype Lift sig m k = Lift { unLift :: sig k }
  deriving (Functor)

instance Functor sig => HFunctor (Lift sig) where
  hfmap _ (Lift op) = Lift op

instance Functor sig => Effect (Lift sig) where
  handle state handler (Lift op) = Lift (fmap (handler . (<$ state)) op)
