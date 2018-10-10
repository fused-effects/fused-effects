{-# LANGUAGE DeriveFunctor, PolyKinds #-}
module Control.Effect.Trace where

data Trace m k = Trace String k
  deriving (Functor)
