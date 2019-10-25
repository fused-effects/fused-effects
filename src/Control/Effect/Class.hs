-- | Provides the 'Effect' class that effect types implement.
--
-- @since 1.0.0.0
module Control.Effect.Class
( -- * 'Effect' class
  Effect(..)
  -- * Generic deriving of 'Effect' instances.
, GEffect(..)
) where

import Control.Algebra.Internal (Effect(..), GEffect(..))
