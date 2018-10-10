{-# LANGUAGE DeriveFunctor, PolyKinds #-}
module Control.Effect.Fail.Internal where

import Control.Effect.Handler

newtype Fail m k = Fail String
  deriving (Functor)

instance HFunctor Fail where
  hfmap _ (Fail s) = Fail s

instance Effect Fail where
  handle _ _ (Fail s) = Fail s
