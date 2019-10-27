{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
{- | 'Unlift' effects allow inner contexts to run actions in outer contexts.

Predefined carriers:

* "Control.Carrier.Unlift"

@since 1.0.0.0
-}
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


-- | Run actions in an outer context.
--
-- This can be used to provide interoperation with @base@ functionality like 'Control.Exception.finally':
--
-- @
-- 'withUnlift' $ \ run -> 'Control.Exception.finally' (run m) (run cleanup)
-- @
--
-- @since 1.0.0.0
withUnlift
  :: Has (Unlift n) sig m
  => (forall ctx . Functor ctx => ctx () -> (forall a . ctx (m a) -> n (ctx a)) -> n (ctx a))
  -> m a
withUnlift with = send (Unlift with pure)
