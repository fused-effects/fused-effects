{-# LANGUAGE DeriveFunctor, ExistentialQuantification, RankNTypes, StandaloneDeriving #-}
module Control.Effect.Intercept where

data Intercept eff m k
  = forall a . Intercept (m a) (forall n x . eff n (n x) -> m a) (a -> k)

deriving instance Functor (Intercept eff m)
