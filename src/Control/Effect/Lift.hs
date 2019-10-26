{-# LANGUAGE ExistentialQuantification, RankNTypes, TypeFamilies #-}
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
, withUnlift
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import Control.Effect.Lift.Internal (Lift(..))
import Data.Functor.Identity

-- | Given a @Lift n@ constraint in a signature carried by @m@, 'sendM'
-- promotes arbitrary actions of type @n a@ to @m a@. It is spiritually
-- similar to @lift@ from the @MonadTrans@ typeclass.
--
-- @since 1.0.0.0
sendM :: (Has (Lift n) sig m, Functor n) => n a -> m a
sendM = send . Lift . fmap pure


data Unlift sig m k
  = forall a . Unlift ((forall a . m a -> sig a) -> m a) (a -> m k)

instance Functor m => Functor (Unlift sig m) where
  fmap f (Unlift with k) = Unlift with (fmap f . k)

instance Effect (Unlift sig) where
  type CanHandle (Unlift sig) ctx = ctx ~ Identity
  handle ctx dst (Unlift with k) = Unlift (\ run -> dst (with (run . fmap runIdentity . dst . (<$ ctx)) <$ ctx)) (dst . fmap k)

withUnlift :: Has (Unlift n) sig m => ((forall a . m a -> n a) -> m a) -> m a
withUnlift with = send (Unlift with pure)
