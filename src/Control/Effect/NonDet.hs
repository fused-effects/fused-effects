{-# LANGUAGE FlexibleContexts #-}
-- | 'Choose' & 'Empty'-based nondeterminism interfaces.
module Control.Effect.NonDet
( -- * NonDet effects
  module Control.Effect.Choose
, module Control.Effect.Empty
, oneOf
, foldMapA
) where

import Control.Carrier
import Control.Effect.Choose
import Control.Effect.Empty
import Data.Coerce

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
oneOf :: (Foldable t, Carrier sig m, Member Choose sig, Member Empty sig) => t a -> m a
oneOf = foldMapA pure

-- | Map a 'Foldable' collection of values into a nondeterministic computation using the supplied action.
foldMapA :: (Foldable t, Carrier sig m, Member Choose sig, Member Empty sig) => (a -> m b) -> t a -> m b
foldMapA f = getChoosing #. foldMap (Choosing #. f)


-- | Compose a function operationally equivalent to 'id' on the left.
--
--   cf https://github.com/fused-effects/diffused-effects/pull/1#discussion_r323560758
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ = coerce
{-# INLINE (#.) #-}
