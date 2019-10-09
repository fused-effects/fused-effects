{-# LANGUAGE ConstraintKinds, TypeFamilies, TypeOperators #-}
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
import Data.Kind (Constraint)

type family Has sub sup m :: Constraint where
  Has (l :+: r) u m = (Has l u m, Has r u m)
  Has t         u m = (Member t u, Carrier u m)


-- | Construct a request for an effect to be interpreted by some handler later on.
send :: (Member eff sig, Carrier sig m) => eff m a -> m a
send = eff . inj
{-# INLINE send #-}
