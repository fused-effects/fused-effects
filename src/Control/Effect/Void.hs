{-# LANGUAGE DeriveFunctor, EmptyCase, KindSignatures, MultiParamTypeClasses #-}
module Control.Effect.Void
( Void
, run
, VoidC(..)
) where

import Control.Effect.Carrier

data Void (m :: * -> *) k
  deriving (Functor)

instance HFunctor Void where
  hmap _ v = case v of {}
  {-# INLINE hmap #-}

instance Effect Void where
  handle _ _ v = case v of {}
  {-# INLINE handle #-}


-- | Run an action exhausted of effects to produce its final result value.
run :: VoidC a -> a
run = runVoidC
{-# INLINE run #-}

newtype VoidC a = VoidC { runVoidC :: a }

instance Functor VoidC where
  fmap f (VoidC a) = VoidC (f a)
  {-# INLINE fmap #-}

instance Applicative VoidC where
  pure = VoidC
  {-# INLINE pure #-}

  VoidC f <*> VoidC a = VoidC (f a)
  {-# INLINE (<*>) #-}

instance Monad VoidC where
  return = pure
  {-# INLINE return #-}

  VoidC a >>= f = f a
  {-# INLINE (>>=) #-}

instance Carrier Void VoidC where
  ret = VoidC
  {-# INLINE ret #-}

  eff v = case v of {}
  {-# INLINE eff #-}
