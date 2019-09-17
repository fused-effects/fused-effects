{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, StandaloneDeriving #-}
module Control.Effect.Cull
( -- * Cull effect
  Cull(..)
, cull
) where

import Control.Carrier.Class

-- | 'Cull' effects are used with 'Choose' to provide control over branching.
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
--   prop> run (runNonDet (runCull (cull (pure a <|> pure b)))) === [a]
--   prop> run (runNonDet (runCull (cull (empty  <|> pure a)))) === [a]
--   prop> run (runNonDet (runCull (cull (pure a <|> pure b) <|> pure c))) === [a, c]
--   prop> run (runNonDet (runCull (cull (asum (map pure (repeat a)))))) === [a]
cull :: (Carrier sig m, Member Cull sig) => m a -> m a
cull m = send (Cull m pure)


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.NonDet
-- >>> import Control.Effect.Pure
-- >>> import Data.Foldable (asum)
