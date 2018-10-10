{-# LANGUAGE EmptyCase, MultiParamTypeClasses #-}
module Control.Effect.Void
( Void
, run
) where

import Control.Effect.Handler
import Control.Effect.Internal
import Control.Effect.Void.Internal

-- | Run an 'Eff' exhausted of effects to produce its final result value.
run :: Eff VoidH a -> a
run = runVoidH . interpret


newtype VoidH a = VoidH { runVoidH :: a }

instance Carrier Void VoidH where
  gen = VoidH
  alg v = case v of {}
