{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
module Control.Effect.NonDet.NonEmpty
( -- * NonDet effect
  NonDet(..)
) where

import GHC.Generics (Generic1)

data NonDet m k
  = Choose (Bool -> m k)
  deriving (Functor, Generic1)
