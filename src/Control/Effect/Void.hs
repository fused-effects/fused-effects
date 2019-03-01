{-# LANGUAGE DeriveAnyClass                                                  #-}
{-# LANGUAGE DeriveFunctor, EmptyCase, KindSignatures, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric                                                   #-}

module Control.Effect.Void
( Void
, run
, VoidC(..)
) where

import GHC.Generics (Generic)
import Control.Effect.Carrier
import Control.Effect.Internal

data Void (m :: * -> *) k
  deriving (Functor, Generic)

instance HFunctor Void

instance Effect Void


-- | Run an 'Eff' exhausted of effects to produce its final result value.
run :: Eff VoidC a -> a
run = runVoidC . interpret
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
