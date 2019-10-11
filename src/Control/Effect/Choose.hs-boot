module Control.Effect.Choose
( Choose(..)
) where

import Control.Effect.Class

newtype Choose m k
  = Choose (Bool -> m k)

instance HFunctor Choose
