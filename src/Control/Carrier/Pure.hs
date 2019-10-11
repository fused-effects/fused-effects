{-# LANGUAGE EmptyCase, MultiParamTypeClasses #-}

-- | A carrier for pure effects, used to kick off a stack of effects with 'run'.
module Control.Carrier.Pure
( -- * Pure effect
  module Control.Effect.Pure
  -- * Pure carrier
, run
, PureC(..)
) where

import Control.Applicative
import Control.Carrier.Class
import Control.Effect.Pure
import Control.Monad.Fix
import Data.Coerce

-- | Run an action exhausted of effects to produce its final result value.
--
-- @since 1.0.0.0
run :: PureC a -> a
run = runPureC
{-# INLINE run #-}

-- | @since 1.0.0.0
newtype PureC a = PureC { runPureC :: a }

instance Functor PureC where
  fmap = coerce
  {-# INLINE fmap #-}

  a <$ _ = pure a
  {-# INLINE (<$) #-}

instance Applicative PureC where
  pure = PureC
  {-# INLINE pure #-}

  (<*>) = coerce
  {-# INLINE (<*>) #-}

  liftA2 = coerce
  {-# INLINE liftA2 #-}

  _ *> b = b
  {-# INLINE (*>) #-}

  a <* _ = a
  {-# INLINE (<*) #-}

instance Monad PureC where
  return = pure
  {-# INLINE return #-}

  PureC a >>= f = f a
  {-# INLINE (>>=) #-}

instance MonadFix PureC where
  mfix f = PureC (fix (runPureC . f))
  {-# INLINE mfix #-}

instance Carrier Pure PureC where
  eff v = case v of {}
  {-# INLINE eff #-}
