{-# LANGUAGE DeriveFunctor, FlexibleContexts, KindSignatures #-}
module Control.Effect.Abort
( -- * Abort effect
  Abort(..)
, abort
  -- * Abort carrier
, runAbort
, AbortC(..)
  -- * Re-exports
, Carrier
, Member
, run
) where

import Control.Applicative (liftA2)
import Control.Effect.Carrier

data Abort (m :: * -> *) k = Abort
  deriving (Functor)

-- | Abort the computation.
abort :: (Carrier sig m, Member Abort sig) => m a
abort = send Abort


runAbort :: AbortC m a -> m (Maybe a)
runAbort = runAbortC

newtype AbortC m a = AbortC { runAbortC :: m (Maybe a) }
  deriving (Functor)

instance Applicative m => Applicative (AbortC m) where
  pure = AbortC . pure . Just
  AbortC f <*> AbortC a = AbortC (liftA2 (<*>) f a)

instance Monad m => Monad (AbortC m) where
  AbortC a >>= f = AbortC (a >>= maybe (pure Nothing) (runAbortC . f))
