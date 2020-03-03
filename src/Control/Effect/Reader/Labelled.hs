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

import Control.Effect.Labelled
import Control.Effect.Reader.Internal

-- | Retrieve the environment value.
--
-- @
-- runReader a ('ask' '>>=' k) = runReader a (k a)
-- @
--
-- @since 0.1.0.0
ask :: forall label r m sig . HasLabelled label (Reader r) sig m => m r
ask = sendLabelled @label (Ask pure)
