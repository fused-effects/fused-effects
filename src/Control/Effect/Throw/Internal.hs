{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
module Control.Effect.Throw.Internal
( Throw(..)
) where

-- | @since 1.0.0.0
newtype Throw e (m :: * -> *) k = Throw e
  deriving (Functor)
