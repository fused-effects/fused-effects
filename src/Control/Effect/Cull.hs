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
  -- * Properties
, cull_pruning
  -- * Re-exports
, Carrier
, Has
, run
) where

import Control.Carrier
import Control.Effect.Choose (Choose, (<|>))

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
-- @
-- 'cull' ('pure' a 'Control.Effect.Choose.<|>' m) 'Control.Effect.Choose.<|>' n = 'pure' a 'Control.Effect.Choose.<|>' n
-- @
--
-- @since 0.1.2.0
cull :: Has Cull sig m => m a -> m a
cull m = send (Cull m pure)


-- Properties

cull_pruning :: (Has Cull sig m, Has Choose sig m) => (n [a] -> n [a] -> prop) -> (m a -> n [a]) -> a -> m a -> m a -> prop
cull_pruning (===) runCull a m n = runCull (cull (pure a <|> m) <|> n) === runCull (pure a <|> n)
