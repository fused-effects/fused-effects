{-# LANGUAGE ExistentialQuantification, RankNTypes, TypeFamilies #-}
module Control.Effect.Unlift
( -- * Unlift effect
  Unlift(..)
, withUnlift
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import Data.Functor.Identity

data Unlift sig m k
  = forall a . Unlift ((forall a . m a -> sig a) -> m a) (a -> m k)

instance Functor m => Functor (Unlift sig m) where
  fmap f (Unlift with k) = Unlift with (fmap f . k)

instance Effect (Unlift sig) where
  type CanHandle (Unlift sig) ctx = ctx ~ Identity
  handle ctx dst (Unlift with k) = Unlift (\ run -> dst (with (run . fmap runIdentity . dst . (<$ ctx)) <$ ctx)) (dst . fmap k)

withUnlift :: Has (Unlift n) sig m => ((forall a . m a -> n a) -> m a) -> m a
withUnlift with = send (Unlift with pure)
