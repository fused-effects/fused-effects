{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric, DerivingStrategies, EmptyCase, KindSignatures, MultiParamTypeClasses #-}
module Control.Effect.Pure
( Pure
, run
, PureC(..)
) where

import Control.Applicative
import Control.Effect.Carrier
import Control.Monad.Fix
import Data.Coerce
import Data.Function (fix)
import GHC.Generics (Generic1)

data Pure (m :: * -> *) k
  deriving stock (Functor, Generic1)
  deriving anyclass (HFunctor, Effect)

-- | Run an action exhausted of effects to produce its final result value.
run :: PureC a -> a
run = runPureC
{-# INLINE run #-}

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
