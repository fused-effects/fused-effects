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

type Has eff sig m = (Has' eff sig, Carrier sig m)

type family Has' sub sup :: Constraint where
  Has' (l :+: r) u = (Has' l u, Has' r u)
  Has' t         u = Member t u


send :: (Member eff sig, Carrier sig m) => eff m a -> m a
