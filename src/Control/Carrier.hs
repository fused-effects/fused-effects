{-# LANGUAGE ConstraintKinds, TypeFamilies, TypeOperators #-}
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
import Data.Kind (Constraint)

-- | The @m@ is a carrier for @sig@ containing @eff@.
type Has eff sig m = (Members eff sig, Carrier sig m)

-- | Decompose sums on the left into multiple 'Member' constraints.
type family Members sub sup :: Constraint where
  Members (l :+: r) u = (Members l u, Members r u)
  Members t         u = Member t u


-- | Construct a request for an effect to be interpreted by some handler later on.
send :: (Member eff sig, Carrier sig m) => eff m a -> m a
send = eff . inj
{-# INLINE send #-}
