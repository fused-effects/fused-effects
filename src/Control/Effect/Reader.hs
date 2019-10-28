{- | An effect providing access to an immutable (but locally-modifiable) context value.

This effect is similar to the traditional @MonadReader@ typeclass, though it allows the presence of multiple @Reader t@ effects.

Predefined carriers:

* "Control.Carrier.Reader".
* "Control.Monad.Trans.Reader".
* "Control.Monad.Trans.RWS.Lazy"
* "Control.Monad.Trans.RWS.Strict"
* If 'Reader' @r@ is the last effect in a stack, it can be interpreted directly to @(-> r)@ (a function taking an @r@).

@since 0.1.0.0
-}

module Control.Effect.Reader
( -- * Reader effect
  Reader(..)
, ask
, asks
, local
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import Control.Effect.Reader.Internal (Reader(..))

-- | Retrieve the environment value.
--
-- @
-- runReader a ('ask' '>>=' k) = runReader a (k a)
-- @
--
-- @since 0.1.0.0
ask :: Has (Reader r) sig m => m r
ask = send (Ask pure)

-- | Project a function out of the current environment value.
--
-- @
-- 'asks' f = 'fmap' f 'ask'
-- @
--
-- @since 0.1.0.0
asks :: Has (Reader r) sig m => (r -> a) -> m a
asks f = send (Ask (pure . f))

-- | Run a computation with an environment value locally modified by the passed function.
--
-- @
-- runReader a ('local' f m) = runReader (f a) m
-- @
--
-- @since 0.1.0.0
local :: Has (Reader r) sig m => (r -> r) -> m a -> m a
local f m = send (Local f m pure)
