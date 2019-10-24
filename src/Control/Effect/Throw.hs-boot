{-# LANGUAGE KindSignatures #-}
module Control.Effect.Throw
( Throw(..)
) where

import Control.Effect.Class

data Throw e (m :: * -> *) k
  = Throw e

instance Effect (Throw e)
