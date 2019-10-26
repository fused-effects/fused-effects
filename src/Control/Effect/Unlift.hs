{-# LANGUAGE ExistentialQuantification, RankNTypes, TypeFamilies #-}
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
import Data.Functor.Identity

-- | @since 1.0.0.0
data Unlift sig m k
  = forall a . Unlift ((forall a . m a -> sig a) -> sig a) (a -> m k)

instance Functor m => Functor (Unlift sig m) where
  fmap f (Unlift with k) = Unlift with (fmap f . k)

instance Effect (Unlift sig) where
  type CanHandle (Unlift sig) ctx = ctx ~ Identity
  handle ctx dst (Unlift with k) = Unlift (\ run -> with (run . fmap runIdentity . dst . (<$ ctx))) (dst . (<$ ctx) . k)


-- | Run actions in an outer context.
--
-- This can be used to provide interoperation with @base@ functionality like 'Control.Exception.finally':
--
-- @
-- 'withUnlift' $ \ run -> 'Control.Exception.finally' (run m) (run cleanup)
-- @
--
-- @since 1.0.0.0
withUnlift :: Has (Unlift n) sig m => ((forall a . m a -> n a) -> n a) -> m a
withUnlift with = send (Unlift with pure)
