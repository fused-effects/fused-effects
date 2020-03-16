{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Control.Effect.Choose.Internal
( Choose(..)
) where

import Data.Kind (Type)

-- | @since 1.0.0.0
data Choose (m :: Type -> Type) k where
  Choose :: Choose m Bool
