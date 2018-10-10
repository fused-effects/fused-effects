{-# LANGUAGE DeriveFunctor, EmptyCase, PolyKinds #-}
module Control.Effect.Void.Internal where

import Control.Effect.Handler

data Void m k
  deriving (Functor)

instance Effect Void where
  hfmap _ v = case v of {}
  handle _ _ v = case v of {}
