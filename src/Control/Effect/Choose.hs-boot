module Control.Effect.Choose
( Choose(..)
) where

import Control.Effect.Class

newtype Choose m k
  = Choose (Bool -> m k)

instance Functor m => Functor (Choose m)
instance HFunctor Choose
instance Effect   Choose
