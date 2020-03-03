{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
module Control.Effect.Empty.Internal
( Empty(..)
) where

import GHC.Generics (Generic1)

-- | @since 1.0.0.0
data Empty (m :: * -> *) k = Empty
  deriving (Functor, Generic1)
