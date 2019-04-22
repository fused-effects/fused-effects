{-# LANGUAGE DeriveFunctor, EmptyCase, KindSignatures, MultiParamTypeClasses #-}
module Control.Effect.Pure
( Pure
, run
, PureC(..)
) where

import Control.Applicative
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

instance Carrier Pure PureC where
  eff v = case v of {}
  {-# INLINE eff #-}
