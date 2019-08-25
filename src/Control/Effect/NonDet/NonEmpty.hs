{-# LANGUAGE DeriveFunctor #-}
module Control.Effect.NonDet.NonEmpty
( -- * NonDet effect
  NonDet(..)
) where

data NonDet m k
  = Choose (Bool -> m k)
  deriving (Functor)
