{-# LANGUAGE DeriveFunctor, EmptyCase, KindSignatures, MultiParamTypeClasses #-}
module Control.Effect.Void
( Void
, run
, VoidC(..)
) where

import Control.Effect.Carrier
import Control.Effect.Internal

data Void (m :: * -> *) k
  deriving (Functor)

instance HFunctor Void where
  hmap _ v = case v of {}

instance Effect Void where
  handle _ _ v = case v of {}


-- | Run an 'Eff' exhausted of effects to produce its final result value.
run :: Eff VoidC a -> a
run = runVoidC . interpret

newtype VoidC a = VoidC { runVoidC :: a }

instance Carrier Void VoidC where
  ret = VoidC
  eff v = case v of {}
