{-# LANGUAGE DeriveFunctor, PolyKinds #-}
module Control.Effect.Void.Internal where

data Void m k
  deriving (Functor)
