module Control.Carrier.Fresh.Church
( -- * Fresh carrier
  FreshC(..)
  -- * Fresh effect
, module Control.Effect.Fresh
) where

import Control.Carrier.State.Church
import Control.Effect.Fresh

newtype FreshC m a = FreshC (StateC Int m a)
