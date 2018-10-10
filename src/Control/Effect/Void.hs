{-# LANGUAGE EmptyCase, MultiParamTypeClasses #-}
module Control.Effect.Void where

import Control.Effect.Internal
import Control.Effect.Void.Internal


-- | Run an 'Eff' exhausted of effects to produce its final result value.
run :: Eff VoidH a -> a
run = runVoidH . runEff VoidH


newtype VoidH a = VoidH { runVoidH :: a }

instance TermAlgebra VoidH Void where
  var = VoidH
  con v = case v of {}
