{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
module Control.Effect.State.Internal
( State(..)
) where

import Control.Effect.Class
import GHC.Generics (Generic1)

-- | @since 0.1.0.0
data State s m k
  = Get (s -> m k)
  | Put s (m k)
  deriving (Functor, Generic1)

instance Functor f => Effect f (State s)
