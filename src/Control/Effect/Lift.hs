{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts #-}

{- | Provides a mechanism to kick off the evaluation of an effect stack that takes place in a monadic context.

'Lift' effects are always the last effect in a given effect stack. These stacks are invoked with 'Control.Effect.Lift.runM'. The 'Control.Effect.Pure.Pure' effect is equivalent to @Lift Identity@.
-}

module Control.Effect.Lift
( -- * Lift effect
  Lift(..)
, sendM
  -- * Re-exports
, Has
) where

import Control.Carrier
import GHC.Generics

-- | @since 1.0.0.0
newtype Lift sig m k = Lift { unLift :: sig (m k) }
  deriving (Functor, Generic1)

instance Functor m => HFunctor (Lift m)
instance Functor m => Effect   (Lift m)

-- | Given a @Lift n@ constraint in a signature carried by @m@, 'sendM'
-- promotes arbitrary actions of type @n a@ to @m a@. It is spiritually
-- similar to @lift@ from the @MonadTrans@ typeclass.
--
-- @since 1.0.0.0
sendM :: (Has (Lift n) sig m, Functor n) => n a -> m a
sendM = send . Lift . fmap pure
