{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, StandaloneDeriving #-}
{- | Provides an effect to cull choices in a given nondeterministic context. This effect is used in concert with 'Control.Effect.NonDet.NonDet'.

Computations run inside a call to 'cull' will return at most one result.

Predefined carriers:

* "Control.Carrier.Cull.Church"
-}
module Control.Effect.Cull
( -- * Cull effect
  Cull(..)
, cull
  -- * Re-exports
, Has
) where

import Control.Carrier

-- | 'Cull' effects are used with 'Choose' to provide control over branching.
--
-- @since 0.1.2.0
data Cull m k
  = forall a . Cull (m a) (a -> m k)

deriving instance Functor m => Functor (Cull m)

instance HFunctor Cull where
  hmap f (Cull m k) = Cull (f m) (f . k)
  {-# INLINE hmap #-}

instance Effect Cull where
  handle state handler (Cull m k) = Cull (handler (m <$ state)) (handler . fmap k)
  {-# INLINE handle #-}

-- | Cull nondeterminism in the argument, returning at most one result.
--
--   prop> run (runNonDetA (runCullA (cull (pure a <|> pure b)))) === [a]
--   prop> run (runNonDetA (runCullA (cull (empty  <|> pure a)))) === [a]
--   prop> run (runNonDetA (runCullA (cull (pure a <|> pure b) <|> pure c))) === [a, c]
--   prop> run (runNonDetA (runCullA (cull (asum (map pure (repeat a)))))) === [a]
--
-- @since 0.1.2.0
cull :: Has Cull sig m => m a -> m a
cull m = send (Cull m pure)


-- $setup
-- >>> import Test.QuickCheck
-- >>> import Control.Carrier.Cull.Church
-- >>> import Control.Carrier.NonDet.Church
-- >>> import Data.Foldable (asum)
