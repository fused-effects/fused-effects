{-# LANGUAGE DeriveFunctor #-}
module Control.Effect.Choose.Internal
( Choose(..)
) where

-- | @since 1.0.0.0
newtype Choose m k
  = Choose (Bool -> m k)
  deriving (Functor)
