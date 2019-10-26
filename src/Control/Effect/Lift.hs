{-# LANGUAGE RankNTypes #-}
{- | Provides a mechanism to kick off the evaluation of an effect stack that takes place in a monadic context.

'Lift' effects are always the last effect in a given effect stack. These stacks are invoked with 'Control.Carrier.Lift.runM' or 'Control.Algebra.run'.

Predefined carriers:

* "Control.Carrier.Lift"
* 'IO'
* 'Data.Functor.Identity.Identity'

@since 0.1.0.0
-}

module Control.Effect.Lift
( -- * Lift effect
  Lift(..)
, sendM
  -- * Unlift effect
, Unlift(..)
, withUnlift
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import Control.Effect.Lift.Internal (Lift(..), Unlift(..))

-- | Given a @Lift n@ constraint in a signature carried by @m@, 'sendM'
-- promotes arbitrary actions of type @n a@ to @m a@. It is spiritually
-- similar to @lift@ from the @MonadTrans@ typeclass.
--
-- @since 1.0.0.0
sendM :: (Has (Lift n) sig m, Functor n) => n a -> m a
sendM = send . Lift . fmap pure


withUnlift :: Has (Unlift n) sig m => ((forall a . m a -> n a) -> m a) -> m a
withUnlift with = send (Unlift with pure)
