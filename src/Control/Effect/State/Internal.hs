{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Control.Effect.State.Internal
( State(..)
) where

import Data.Kind (Type)

-- | @since 0.1.0.0
data State s (m :: Type -> Type) k where
  Get ::      State s m s
  Put :: s -> State s m ()
