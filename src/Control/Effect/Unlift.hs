{-# LANGUAGE RankNTypes #-}
{- | 'Unlift' effects allow inner contexts to run actions in outer contexts.

Predefined carriers:

* "Control.Carrier.Unlift"

@since 1.0.0.0
-}
module Control.Effect.Unlift
( -- * Unlift effect
  Unlift(..)
, liftWith
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
-- 'liftWith' $ \ run -> 'Control.Exception.finally' (run m) (run cleanup)
-- @
--
-- @since 1.0.0.0
liftWith
  :: Has (Unlift n) sig m
  => (forall ctx . Functor ctx => ctx () -> (forall a . ctx (m a) -> n (ctx a)) -> n (ctx a))
  -> m a
liftWith with = send (Unlift with pure)
