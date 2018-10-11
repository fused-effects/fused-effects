{-# LANGUAGE DeriveFunctor, ExistentialQuantification, PolyKinds, StandaloneDeriving #-}
module Control.Effect.Fresh where

data Fresh m k
  = Fresh (Int -> k)
  | forall b . Reset Int (m b) (b -> k)

deriving instance Functor (Fresh m)
