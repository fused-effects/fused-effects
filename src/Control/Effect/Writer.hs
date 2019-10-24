{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, StandaloneDeriving #-}

{- | An effect allowing writes to an accumulated quantity alongside a computed value. A 'Writer' @w@ effect keeps track of a monoidal datum of type @w@ and strictly appends to that monoidal value with the 'tell' effect. Writes to that value can be detected and intercepted with the 'listen' and 'censor' effects.

Predefined carriers:

* "Control.Carrier.Writer.Strict". (A lazy carrier is not provided due to the inherent space leaks associated with lazy writer monads.)
* "Control.Monad.Trans.RWS.Lazy"
* "Control.Monad.Trans.RWS.Strict"
* "Control.Monad.Trans.Writer.Lazy"
* "Control.Monad.Trans.Writer.Strict"
* If 'Writer' @w@ is the last effect in a stack, it can be interpreted to a tuple @(w, a)@ given some result type @a@ and the presence of a 'Monoid' instance for @w@.

@since 0.1.0.0
-}

module Control.Effect.Writer
( -- * Writer effect
  Writer(..)
, tell
, listen
, listens
, censor
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import Control.Effect.Writer.Internal (Writer(..))

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
