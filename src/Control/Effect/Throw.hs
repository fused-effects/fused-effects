{-# LANGUAGE DeriveFunctor, KindSignatures #-}
module Control.Effect.Throw
( -- * Throw effect
  Throw(..)
) where

data Throw e (m :: * -> *) k
  = Throw e
  deriving (Functor)
