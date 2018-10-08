{-# LANGUAGE FunctionalDependencies #-}
module Control.Algebra where

class TermAlgebra h f | h -> f where
  var :: a -> h a
  con :: f (h a) -> h a
