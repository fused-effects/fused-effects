module Control.Effect.NonDet
( -- * NonDet effects
  module Control.Effect.Choose
, module Control.Effect.Empty
, oneOf
  -- * Re-exports
, Alternative(..)
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Choose hiding ((<|>), many, some)
import Control.Effect.Empty hiding (empty)
import Data.Monoid (Alt(..))

-- | Nondeterministically choose an element from a 'Foldable' collection.
-- This can be used to emulate the style of nondeterminism associated with
-- programming in the list monad:
-- @
--   pythagoreanTriples = do
--     a <- oneOf [1..10]
--     b <- oneOf [1..10]
--     c <- oneOf [1..10]
--     guard (a^2 + b^2 == c^2)
--     pure (a, b, c)
-- @
oneOf :: (Foldable t, Alternative m) => t a -> m a
oneOf = getAlt . foldMap (Alt . pure)
