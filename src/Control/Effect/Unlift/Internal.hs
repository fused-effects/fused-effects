{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Control.Effect.Unlift.Internal
( Unlift(..)
) where

import Control.Effect.Class
import Data.Functor.Compose

-- | @since 1.0.0.0
data Unlift sig m k
  = forall a . Unlift
    (forall ctx . Functor ctx => ctx () -> (forall a . ctx (m a) -> sig (ctx a)) -> sig (ctx a))
    (a -> m k)

instance Functor m => Functor (Unlift sig m) where
  fmap f (Unlift with k) = Unlift with (fmap f . k)

instance Functor sig => Effect (Unlift sig) where
  handle ctx dst (Unlift with k) = Unlift (\ ctx' dst' -> getCompose <$> with (Compose (ctx <$ ctx')) (fmap Compose . dst' . fmap dst . getCompose)) (dst . fmap k)
