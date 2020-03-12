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
, sendIO
, liftWith
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import Control.Effect.Lift.Internal (Lift(..))

-- | Given a @Lift n@ constraint in a signature carried by @m@, 'sendM'
-- promotes arbitrary actions of type @n a@ to @m a@. It is spiritually
-- similar to @lift@ from the @MonadTrans@ typeclass.
--
-- @since 1.0.0.0
sendM :: (Has (Lift n) sig m, Functor n) => n a -> m a
sendM m = send (LiftWith (\ _ ctx -> (<$ ctx) <$> m) pure)

-- | A type-restricted variant of 'sendM' for 'IO' actions.
--
-- This is particularly useful when you have a @'Has' ('Lift' 'IO') sig m@ constraint for the use of 'liftWith', and want to run an action abstracted over 'Control.Monad.IO.Class.MonadIO'. 'IO' has a 'Control.Monad.IO.Class.MonadIO' instance, and 'sendIO'’s type restricts the action’s type to 'IO' without further type annotations.
--
-- @since 1.0.2.0
sendIO :: Has (Lift IO) sig m => IO a -> m a
sendIO = sendM


-- | Run actions in an outer context.
--
-- This can be used to provide interoperation with @base@ functionality like @"Control.Exception".'Control.Exception.catch'@:
--
-- @
-- 'liftWith' $ \\ hdl ctx -> 'Control.Exception.catch' (hdl (m <$ ctx)) (hdl . (<$ ctx) . h)
-- @
--
-- The higher-order function takes both an initial context, and a handler phrased as a distributive law (as described in the documentation for 'Handler'). This handler takes actions lifted into a context functor, which can be either the initial context, or the derived context produced by handling a previous action.
--
-- As with @MonadBaseControl@, care must be taken when lifting functions like @"Control.Exception".'Control.Exception.finally'@ which don’t use the return value of one of their actions, as this can lead to dropped effects.
--
-- @since 1.0.0.0
liftWith
  :: Has (Lift n) sig m
  => (forall ctx . Functor ctx => Handler ctx m n -> ctx () -> n (ctx a))
  -> m a
liftWith with = send (LiftWith with pure)
