{-# LANGUAGE ConstraintKinds #-}
module Control.Carrier
( -- * Effect requests
  Has
, send
  -- * Re-exports
, module Control.Carrier.Class
, module Control.Carrier.Pure
, module Control.Effect.Class
, (:+:)(..)
) where

import {-# SOURCE #-} Control.Carrier.Class
import Control.Carrier.Pure
import Control.Effect.Class
import Control.Effect.Sum

type Has eff sig m = (Members eff sig, Carrier sig m)

send :: (Member eff sig, Carrier sig m) => eff m a -> m a
