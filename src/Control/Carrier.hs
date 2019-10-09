{-# LANGUAGE ConstraintKinds #-}
module Control.Carrier
( -- * Re-exports
  module Control.Carrier.Class
, module Control.Carrier.Pure
, module Control.Effect.Class
, (:+:)(..)
, Has
, send
) where

import {-# SOURCE #-} Control.Carrier.Class
import Control.Carrier.Pure
import Control.Effect.Class
import Control.Effect.Sum

type Has eff sig m = (Member eff sig, Carrier sig m)

-- | Construct a request for an effect to be interpreted by some handler later on.
send :: Has eff sig m => eff m a -> m a
send = eff . inj
{-# INLINE send #-}
