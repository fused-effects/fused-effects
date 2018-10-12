{-# LANGUAGE DeriveFunctor, PolyKinds #-}
module Control.Effect.Fail.Internal
( Fail(..)
) where

import Control.Effect.Handler

newtype Fail m k = Fail String
  deriving (Functor)

instance HFunctor Fail where
  hmap _ (Fail s) = Fail s

instance Effect Fail where
  handle _ _ (Fail s) = Fail s
