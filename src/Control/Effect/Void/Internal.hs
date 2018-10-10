{-# LANGUAGE DeriveFunctor, EmptyCase, PolyKinds #-}
module Control.Effect.Void.Internal where

import Control.Effect.Handler

data Void m k
  deriving (Functor)

instance HFunctor Void where
  hfmap _ v = case v of {}

instance Effect Void where
  handle _ _ v = case v of {}
