{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, StandaloneDeriving #-}

{- | An effect allowing writes to an accumulated quantity alongside a computed value. A 'Writer' @w@ effect keeps track of a monoidal datum of type @w@ and strictly appends to that monoidal value with the 'tell' effect. Writes to that value can be detected and intercepted with the 'listen' and 'censor' effects.

Predefined carriers:

* "Control.Carrier.Writer.Strict". (A lazy carrier is not provided due to the inherent space leaks associated with lazy writer monads.)
* If 'Writer' @w@ is the last effect in a stack, it can be interpreted to a tuple @(w, a)@ given some result type @a@ and the presence of a 'Monoid' instance for @w@.
-}

module Control.Effect.Writer
( -- * Writer effect
  Writer(..)
, tell
, listen
, listens
, censor
  -- * Re-exports
, Has
) where

import Control.Carrier

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
--   prop> fst (run (runWriter (mapM_ (tell . Sum) (0 : ws)))) === foldMap Sum ws
--
-- @since 0.1.0.0
tell :: Has (Writer w) sig m => w -> m ()
tell w = send (Tell w (pure ()))
{-# INLINE tell #-}

-- | Run a computation, returning the pair of its output and its result.
--
--   prop> run (runWriter (fst <$ tell (Sum a) <*> listen @(Sum Integer) (tell (Sum b)))) === (Sum a <> Sum b, Sum b)
--
-- @since 0.2.0.0
listen :: Has (Writer w) sig m => m a -> m (w, a)
listen m = send (Listen m (curry pure))
{-# INLINE listen #-}

-- | Run a computation, applying a function to its output and returning the pair of the modified output and its result.
--
--   prop> run (runWriter (fst <$ tell (Sum a) <*> listens @(Sum Integer) (applyFun f) (tell (Sum b)))) === (Sum a <> Sum b, applyFun f (Sum b))
--
-- @since 0.2.0.0
listens :: Has (Writer w) sig m => (w -> b) -> m a -> m (b, a)
listens f m = send (Listen m (curry pure . f))
{-# INLINE listens #-}

-- | Run a computation, modifying its output with the passed function.
--
--   prop> run (execWriter (censor (applyFun f) (tell (Sum a)))) === applyFun f (Sum a)
--   prop> run (execWriter (tell (Sum a) *> censor (applyFun f) (tell (Sum b)) *> tell (Sum c))) === (Sum a <> applyFun f (Sum b) <> Sum c)
--
-- @since 0.2.0.0
censor :: Has (Writer w) sig m => (w -> w) -> m a -> m a
censor f m = send (Censor f m pure)
{-# INLINE censor #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> :seti -XTypeApplications
-- >>> import Control.Carrier.Writer.Strict
-- >>> import Test.QuickCheck
-- >>> import Data.Semigroup (Semigroup(..), Sum(..))
