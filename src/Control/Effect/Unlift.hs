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
import Control.Effect.Unlift.Internal (Unlift(..))

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
