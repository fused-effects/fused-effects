{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Control.Effect.State.Internal
( State(..)
) where

import GHC.Generics (Generic1)

-- | @since 0.1.0.0
data State s m k
  = Get (s -> m k)
  | Put s (m k)
  deriving (Functor, Generic1)
