{-# LANGUAGE DeriveFunctor, PolyKinds, TypeOperators #-}
module Control.Effect.Sum.Internal where

import Control.Effect.Handler

data (f :+: g) m k
  = L (f m k)
  | R (g m k)
  deriving (Eq, Ord, Show)

infixr 4 :+:

instance (Effect l, Effect r) => Effect (l :+: r) where
  hfmap f (L l) = L (hfmap f l)
  hfmap f (R r) = R (hfmap f r)

  fmap' f (L l) = L (fmap' f l)
  fmap' f (R r) = R (fmap' f r)

  handle state handler (L l) = L (handle state handler l)
  handle state handler (R r) = R (handle state handler r)
