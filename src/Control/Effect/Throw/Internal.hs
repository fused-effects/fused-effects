{-# LANGUAGE DeriveFunctor, DeriveGeneric, KindSignatures, MultiParamTypeClasses #-}
module Control.Effect.Throw.Internal
( Throw(..)
) where

import Control.Effect.Class
import GHC.Generics (Generic1)

-- | @since 1.0.0.0
data Throw e (m :: * -> *) k
  = Throw e
  deriving (Functor, Generic1)

instance Effect Functor (Throw e)
