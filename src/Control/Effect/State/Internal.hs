{-# LANGUAGE DeriveFunctor #-}
module Control.Effect.State.Internal
( State(..)
) where

-- | @since 0.1.0.0
data State s m k
  = Get (s -> m k)
  | Put s (m k)
  deriving (Functor)
