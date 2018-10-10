{-# LANGUAGE DeriveFunctor, PolyKinds #-}
module Control.Effect.Fail.Internal where

import Control.Effect.Handler

newtype Fail m k = Fail String
  deriving (Functor)

instance Effect Fail where
  hfmap _ (Fail s) = Fail s

  handle _ _ (Fail s) = Fail s
