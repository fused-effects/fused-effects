{-# LANGUAGE DeriveFunctor, EmptyCase, MultiParamTypeClasses, PolyKinds #-}
module Control.Effect.Void
( Void
, run
) where

import Control.Effect.Handler
import Control.Effect.Internal

data Void m k
  deriving (Functor)

instance HFunctor Void where
  hfmap _ v = case v of {}

instance Effect Void where
  handle _ _ v = case v of {}


-- | Run an 'Eff' exhausted of effects to produce its final result value.
run :: Eff VoidH a -> a
run = runVoidH . interpret

newtype VoidH a = VoidH { runVoidH :: a }

instance Carrier Void VoidH where
  gen = VoidH
  alg v = case v of {}
