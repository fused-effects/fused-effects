module Control.Carrier
( -- * Re-exports
  module Control.Carrier.Class
, module Control.Carrier.Pure
, module Control.Effect.Class
, module Control.Effect.Sum
, send
) where

import Control.Carrier.Class
import Control.Carrier.Pure
import Control.Effect.Class
import Control.Effect.Sum

-- | Construct a request for an effect to be interpreted by some handler later on.
send :: (Inject effect sig, Carrier sig m) => effect m a -> m a
send = eff . inj
{-# INLINE send #-}
