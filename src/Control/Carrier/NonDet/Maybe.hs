{-# LANGUAGE DeriveFunctor #-}
module Control.Carrier.NonDet.Maybe
( -- * NonDet effects
  module Control.Effect.NonDet
  -- * NonDet carrier
, runNonDet
, NonDetC(..)
  -- * Re-exports
, Carrier
, Has
, run
) where

import Control.Applicative (liftA2)
import Control.Carrier
import Control.Effect.NonDet

runNonDet :: NonDetC m a -> m (Maybe a)
runNonDet = runNonDetC

newtype NonDetC m a = NonDetC { runNonDetC :: m (Maybe a) }
  deriving (Functor)

instance Applicative m => Applicative (NonDetC m) where
  pure = NonDetC . pure . Just
  {-# INLINE pure #-}
  NonDetC f <*> NonDetC a = NonDetC (liftA2 (<*>) f a)
  {-# INLINE (<*>) #-}

instance Monad m => Monad (NonDetC m) where
  NonDetC a >>= f = NonDetC (a >>= maybe (pure Nothing) (runNonDet . f))
  {-# INLINE (>>=) #-}
