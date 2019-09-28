{-# LANGUAGE TypeOperators #-}
module Control.Effect.NonDet
( -- * NonDet effects
  module Control.Effect.Choose
, module Control.Effect.Empty
, NonDet
, oneOf
, foldMapA
  -- * Re-exports
, Alternative(..)
, guard
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Choose hiding ((<|>), many, some)
import Control.Effect.Empty hiding (empty, guard)
import Control.Effect.Sum
import Control.Monad (guard)
import Data.Coerce
import Data.Monoid (Alt(..))

type NonDet = Empty :+: Choose

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
oneOf = foldMapA pure

-- | Map a 'Foldable' collection of values into a nondeterministic computation using the supplied action.
foldMapA :: (Foldable t, Alternative m) => (a -> m b) -> t a -> m b
foldMapA f = getAlt #. foldMap (Alt #. f)


-- | Compose a function operationally equivalent to 'id' on the left.
--
--   cf https://github.com/fused-effects/diffused-effects/pull/1#discussion_r323560758
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ = coerce
{-# INLINE (#.) #-}
