{-# LANGUAGE DeriveFunctor, DeriveGeneric, RankNTypes #-}
module Control.Effect.NonDet.NonEmpty
( -- * NonDet effect
  NonDet(..)
  -- * NonDet carrier
, NonDetC(..)
) where

import Control.Effect.Carrier
import GHC.Generics (Generic1)

data NonDet m k
  = Choose (Bool -> m k)
  deriving (Functor, Generic1)

instance HFunctor NonDet
instance Effect   NonDet


-- | A carrier for 'NonDet' effects based on Ralf Hinze’s design described in [Deriving Backtracking Monad Transformers](https://www.cs.ox.ac.uk/ralf.hinze/publications/#P12).
newtype NonDetC m a = NonDetC
  { -- | A higher-order function receiving three continuations, respectively implementing '<|>' and 'pure'.
    runNonDetC :: forall b . (m b -> m b -> m b) -> (a -> m b) -> m b
  }
  deriving (Functor)
