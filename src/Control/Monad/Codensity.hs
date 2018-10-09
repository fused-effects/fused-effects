{-# LANGUAGE RankNTypes #-}
module Control.Monad.Codensity where

import Control.Monad (ap, liftM)

newtype Codensity h a = Codensity { unCodensity :: forall x . (a -> h x) -> h x }

runCodensity :: (a -> f x) -> Codensity f a -> f x
runCodensity = flip unCodensity

instance Functor (Codensity h) where
  fmap = liftM

instance Applicative (Codensity h) where
  pure a = Codensity ($ a)

  (<*>) = ap

instance Monad (Codensity h) where
  return = pure

  Codensity m >>= f = Codensity (\ k -> m (runCodensity k . f))
