{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Control.Effect.Reader.Labelled
( -- * Reader effect
  Reader
, ask
  -- * Re-exports
, Algebra
, Effect
, Has
, run
) where

import           Control.Effect.Labelled
import qualified Control.Effect.Reader as R
import           Control.Effect.Reader.Internal

-- | Retrieve the environment value.
--
-- @
-- runReader a ('runLabelled' @_ @label ('ask' @label) '>>=' k) = runReader a (k a)
-- @
--
-- @since 1.0.2.0
ask :: forall label r m sig . HasLabelled label (Reader r) sig m => m r
ask = runUnderLabel @_ @label R.ask
