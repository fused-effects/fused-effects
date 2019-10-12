{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, StandaloneDeriving #-}

{- | An effect providing access to an immutable (but locally-modifiable) context value.

This effect is similar to the traditional @MonadReader@ typeclass, though it allows the presence of multiple @Reader t@ effects.

Predefined carriers:

* "Control.Carrier.Reader.Function".
* If 'Reader' @r@ is the last effect in a stack, it can be interpreted directly to @(-> r)@ (a function taking an @r@).
-}

module Control.Effect.Reader
( -- * Reader effect
  Reader(..)
, ask
, asks
, local
  -- * Properties
, ask_environment
, local_modification
  -- * Re-exports
, Has
) where

import Control.Carrier

-- | @since 0.1.0.0
data Reader r m k
  = Ask (r -> m k)
  | forall b . Local (r -> r) (m b) (b -> m k)

deriving instance Functor m => Functor (Reader r m)

instance HFunctor (Reader r) where
  hmap f (Ask k)       = Ask           (f . k)
  hmap f (Local g m k) = Local g (f m) (f . k)

instance Effect (Reader r) where
  handle state handler (Ask k)       = Ask (handler . (<$ state) . k)
  handle state handler (Local f m k) = Local f (handler (m <$ state)) (handler . fmap k)

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


-- Properties

-- | 'ask' returns the environment variable.
--
-- @since 1.0.0.0
ask_environment :: Has (Reader r) sig m => (b -> b -> prop) -> (r -> m a -> b) -> r -> (r -> m a) -> prop
ask_environment (===) runReader a k = runReader a (ask >>= k) === runReader a (k a)

-- | 'local' modifies the environment variable.
--
-- @since 1.0.0.0
local_modification :: Has (Reader r) sig m => (b -> b -> prop) -> (r -> m a -> b) -> r -> (r -> r) -> m a -> prop
local_modification (===) runReader a f m = runReader a (local f m) === runReader (f a) m
