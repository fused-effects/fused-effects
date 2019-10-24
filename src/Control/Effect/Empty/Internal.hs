{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleInstances, KindSignatures, MultiParamTypeClasses #-}
module Control.Effect.Empty.Internal
( Empty(..)
) where

import Control.Effect.Class
import GHC.Generics (Generic1)

-- | @since 1.0.0.0
data Empty (m :: * -> *) k = Empty
  deriving (Functor, Generic1)

instance Functor f => Effect f Empty
