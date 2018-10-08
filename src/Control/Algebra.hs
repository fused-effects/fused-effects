{-# LANGUAGE FlexibleInstances, FunctionalDependencies #-}
module Control.Algebra where

import Control.Effect

class Effect f => TermAlgebra h f | h -> f where
  var :: a -> h a
  con :: f h (h a) -> h a

instance Effect sig => TermAlgebra (Eff sig) sig where
  var = Return
  con = Eff
