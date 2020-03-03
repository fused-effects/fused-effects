{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
module Control.Effect.Empty.Internal
( Empty(..)
) where

-- | @since 1.0.0.0
data Empty (m :: * -> *) k = Empty
  deriving (Functor)
