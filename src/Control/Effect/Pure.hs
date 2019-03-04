{-# LANGUAGE DeriveFunctor, EmptyCase, KindSignatures, MultiParamTypeClasses #-}
module Control.Effect.Pure
( Pure
, run
, VoidC(..)
) where

import Control.Effect.Carrier
import Data.Coerce

data Pure (m :: * -> *) k
  deriving (Functor)

instance HFunctor Pure where
  hmap _ v = case v of {}
  {-# INLINE hmap #-}

instance Effect Pure where
  handle _ _ v = case v of {}
  {-# INLINE handle #-}


-- | Run an action exhausted of effects to produce its final result value.
run :: VoidC a -> a
run = runVoidC
{-# INLINE run #-}

newtype VoidC a = VoidC { runVoidC :: a }

instance Functor VoidC where
  fmap = coerce
  {-# INLINE fmap #-}

instance Applicative VoidC where
  pure = VoidC
  {-# INLINE pure #-}

  (<*>) = coerce
  {-# INLINE (<*>) #-}

instance Monad VoidC where
  return = pure
  {-# INLINE return #-}

  VoidC a >>= f = f a
  {-# INLINE (>>=) #-}

instance Carrier Pure VoidC where
  eff v = case v of {}
  {-# INLINE eff #-}
