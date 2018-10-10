{-# LANGUAGE DeriveFunctor, PolyKinds #-}
module Control.Effect.NonDet.Internal where

data NonDet m k
  = Empty
  | Choose (Bool -> k)
  deriving (Functor)
