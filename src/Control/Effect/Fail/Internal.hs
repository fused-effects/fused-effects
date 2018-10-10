{-# LANGUAGE DeriveFunctor, PolyKinds #-}
module Control.Effect.Fail.Internal where

newtype Fail m k = Fail String
  deriving (Functor)
