{-# LANGUAGE DeriveFunctor, PolyKinds #-}
module Control.Effect.Lift.Internal where

newtype Lift sig m k = Lift { unLift :: sig k }
  deriving (Functor)
