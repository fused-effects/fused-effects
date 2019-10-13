{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, RankNTypes, StandaloneDeriving #-}

{- | An effect allowing writes to an accumulated quantity alongside a computed value. A 'Writer' @w@ effect keeps track of a monoidal datum of type @w@ and strictly appends to that monoidal value with the 'tell' effect. Writes to that value can be detected and intercepted with the 'listen' and 'censor' effects.

Predefined carriers:

* "Control.Carrier.Writer.Strict". (A lazy carrier is not provided due to the inherent space leaks associated with lazy writer monads.)
* "Control.Monad.Trans.RWS.Lazy"
* "Control.Monad.Trans.RWS.Strict"
* "Control.Monad.Trans.Writer.Lazy"
* "Control.Monad.Trans.Writer.Strict"
* If 'Writer' @w@ is the last effect in a stack, it can be interpreted to a tuple @(w, a)@ given some result type @a@ and the presence of a 'Monoid' instance for @w@.
-}

module Control.Effect.Writer
( -- * Writer effect
  Writer(..)
, tell
, listen
, listens
, censor
  -- * Properties
, tell_append
, listen_eavesdrop
, censor_revision
  -- * Re-exports
, Carrier
, Has
, run
) where

import Control.Arrow ((&&&))
import Control.Carrier
import Data.Bifunctor (first)

-- | @since 0.1.0.0
data Writer w m k
  = Tell w (m k)
  | forall a . Listen (m a) (w -> a -> m k)
  | forall a . Censor (w -> w) (m a) (a -> m k)

deriving instance Functor m => Functor (Writer w m)

instance HFunctor (Writer w) where
  hmap f (Tell w     k) = Tell w         (f       k)
  hmap f (Listen   m k) = Listen   (f m) ((f .) . k)
  hmap f (Censor g m k) = Censor g (f m) (f     . k)
  {-# INLINE hmap #-}

instance Effect (Writer w) where
  handle state handler (Tell w     k) = Tell w                          (handler (k <$ state))
  handle state handler (Listen   m k) = Listen   (handler (m <$ state)) (fmap handler . fmap . k)
  handle state handler (Censor f m k) = Censor f (handler (m <$ state)) (handler . fmap k)
  {-# INLINE handle #-}

-- | Write a value to the log.
--
-- @
-- runWriter ('tell' w '>>' m) = 'first' ('mappend' w) '<$>' runWriter m
-- @
--
-- @since 0.1.0.0
tell :: Has (Writer w) sig m => w -> m ()
tell w = send (Tell w (pure ()))
{-# INLINE tell #-}

-- | Run a computation, returning the pair of its output and its result.
--
-- @
-- runWriter ('listen' m) = 'fmap' ('fst' '&&&' 'id') (runWriter m)
-- @
--
-- @since 0.2.0.0
listen :: Has (Writer w) sig m => m a -> m (w, a)
listen m = send (Listen m (curry pure))
{-# INLINE listen #-}

-- | Run a computation, applying a function to its output and returning the pair of the modified output and its result.
--
-- @
-- 'listens' f m = 'fmap' ('first' f) ('listen' m)
-- @
--
-- @since 0.2.0.0
listens :: Has (Writer w) sig m => (w -> b) -> m a -> m (b, a)
listens f m = send (Listen m (curry pure . f))
{-# INLINE listens #-}

-- | Run a computation, modifying its output with the passed function.
--
-- @
-- runWriter ('censor' f m) = 'fmap' ('first' f) (runWriter m)
-- @
--
-- @since 0.2.0.0
censor :: Has (Writer w) sig m => (w -> w) -> m a -> m a
censor f m = send (Censor f m pure)
{-# INLINE censor #-}


-- Properties

-- | 'tell' appends a value to the log.
--
-- @since 1.0.0.0
tell_append :: (Has (Writer w) sig m, Monoid w, Functor n) => (n (w, b) -> n (w, b) -> prop) -> (m a -> n (w, b)) -> w -> m a -> prop
tell_append (===) runWriter w m = runWriter (tell w >> m) === fmap (first (mappend w)) (runWriter m)

-- | 'listen' eavesdrops on written output.
--
-- @since 1.0.0.0
listen_eavesdrop :: (Has (Writer w) sig m, Functor n) => (n (w, (w, a)) -> n (w, (w, a)) -> prop) -> (forall a . m a -> n (w, a)) -> m a -> prop
listen_eavesdrop (===) runWriter m = runWriter (listen m) === fmap (fst &&& id) (runWriter m)

-- | 'censor' revises written output.
--
-- @since 1.0.0.0
censor_revision :: (Has (Writer w) sig m, Functor n) => (n (w, a) -> n (w, a) -> prop) -> (m a -> n (w, a)) -> (w -> w) -> m a -> prop
censor_revision (===) runWriter f m = runWriter (censor f m) === fmap (first f) (runWriter m)
