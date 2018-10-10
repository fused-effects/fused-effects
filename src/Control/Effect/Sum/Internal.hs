{-# LANGUAGE DeriveFunctor, PolyKinds, TypeOperators #-}
module Control.Effect.Sum.Internal where

data (f :+: g) m k
  = L (f m k)
  | R (g m k)
  deriving (Eq, Ord, Show)

infixr 4 :+:
