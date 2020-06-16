{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
module Control.Effect.Throw.Internal
( Throw(..)
) where

import Data.Kind (Type)

-- | @since 1.0.0.0
newtype Throw e (m :: Type -> Type) k where
  Throw :: e -> Throw e m a
