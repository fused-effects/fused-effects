{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | Labelled 'Reader' operations.
--
-- @since 1.0.2.0
module Control.Effect.Reader.Labelled
( -- * Reader effect
  Reader
, ask
, asks
, local
  -- * Re-exports
, Algebra
, Has
, run
) where

import           Control.Effect.Labelled
import qualified Control.Effect.Reader as R
import           Control.Effect.Reader.Internal

-- | Retrieve the environment value.
--
-- @
-- runReader a ('runLabelled' @label ('ask' @label) '>>=' k) = runReader a (k a)
-- @
--
-- @since 1.0.2.0
ask :: forall label r m sig . HasLabelled label (Reader r) sig m => m r
ask = runUnderLabel @label R.ask
{-# INLINE ask #-}

-- | Project a function out of the current environment value.
--
-- @
-- 'asks' @label f = 'fmap' f ('ask' @label)
-- @
--
-- @since 1.0.2.0
asks :: forall label r m a sig . HasLabelled label (Reader r) sig m => (r -> a) -> m a
asks f = runUnderLabel @label (R.asks f)
{-# INLINE asks #-}

-- | Run a computation with an environment value locally modified by the passed function.
--
-- @
-- runReader a ('runLabelled' @label ('local' @label f m)) = runReader (f a) m
-- @
--
-- @since 1.0.2.0
local :: forall label r m a sig . HasLabelled label (Reader r) sig m => (r -> r) -> m a -> m a
local f m = runUnderLabel @label (R.local f (UnderLabel m))
{-# INLINE local #-}
