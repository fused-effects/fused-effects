{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts, FlexibleInstances, KindSignatures, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
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
import GHC.Generics (Generic1)

data Abort (m :: * -> *) k = Abort
  deriving (Functor, Generic1)

instance HFunctor Abort
instance Effect Abort

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

instance (Carrier sig m, Effect sig) => Carrier (Abort :+: sig) (AbortC m) where
  eff (L Abort) = AbortC (pure Nothing)
  eff (R other) = AbortC (eff (handle (Just ()) (maybe (pure Nothing) runAbortC) other))
