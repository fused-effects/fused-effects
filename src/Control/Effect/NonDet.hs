{-# LANGUAGE TypeOperators #-}

{- | An effect modelling nondeterminism with choice and failure.

Nondeterministic operations are encapsulated by the 'Alternative' class, where 'empty' represents failure and '<|>' represents choice. This module reexports the 'Alternative' interface. If you can't or don't want to use 'Alternative', you can use the 'Control.Effect.Empty.empty' and 'Control.Effect.Choose.<|>' effects (from 'Control.Effect.Empty' and 'Control.Effect.Choose') directly, as the 'NonDet' effect is the composition of 'Choose' and 'Empty'.

Predefined carriers:

* "Control.Carrier.NonDet.Church", which collects all branches' results using an @Alternative@ functor.
* "Control.Carrier.NonDet.Maybe", which terminates upon encountering the first successful result.
* If 'NonDet' is the last effect in a stack, it can be interpreted directly to an @[]@ value.
-}

module Control.Effect.NonDet
( -- * NonDet effects
  NonDet
, oneOf
, foldMapA
  -- * Re-exports
, Alternative(..)
, guard
, optional
, MonadPlus(..)
  -- * Constituent effects
, Control.Effect.Choose.Choose (..)
, Control.Effect.Empty.Empty (..)
) where

import Control.Applicative (Alternative(..), optional)
import Control.Effect.Choose hiding ((<|>), many, some, optional)
import Control.Effect.Empty hiding (empty, guard)
import Control.Effect.Sum
import Control.Monad (MonadPlus(..), guard)
import Data.Coerce
import Data.Monoid (Alt(..))

-- | The nondeterminism effect is the composition of 'Empty' and 'Choose' effects.
type NonDet = Empty :+: Choose

-- | Nondeterministically choose an element from a 'Foldable' collection.
-- This can be used to emulate the style of nondeterminism associated with
-- programming in the list monad:
--
-- @
--   pythagoreanTriples = do
--     a <- oneOf [1..10]
--     b <- oneOf [1..10]
--     c <- oneOf [1..10]
--     guard (a^2 + b^2 == c^2)
--     pure (a, b, c)
-- @
--
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
