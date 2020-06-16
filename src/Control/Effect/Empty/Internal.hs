{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
module Control.Effect.Empty.Internal
( Empty(..)
) where

import Data.Kind (Type)

-- | @since 1.0.0.0
data Empty (m :: Type -> Type) k where
  Empty :: Empty m a
