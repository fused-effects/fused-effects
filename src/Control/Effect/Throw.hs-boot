{-# LANGUAGE FlexibleInstances, KindSignatures, MultiParamTypeClasses #-}
module Control.Effect.Throw
( Throw(..)
) where

import Control.Algebra.Internal

data Throw e (m :: * -> *) k
  = Throw e

instance Functor f => Effect f (Throw e)
