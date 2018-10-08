{-# LANGUAGE RankNTypes #-}
module Control.Monad.Codensity where

newtype Codensity h a = Codensity { runCodensity :: forall x . (a -> h x) -> h x }
