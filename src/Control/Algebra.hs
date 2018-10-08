{-# LANGUAGE FlexibleInstances, FunctionalDependencies #-}
module Control.Algebra where

import Control.Effect

class TermAlgebra h f | h -> f where
  var :: a -> h a
  con :: f h (h a) -> h a

instance TermAlgebra (Eff sig) sig where
  var = Return
  con = Eff
