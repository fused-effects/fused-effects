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

import Control.Applicative (Alternative (..), liftA2)
import Control.Effect.Carrier
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.Trans.Class
import GHC.Generics (Generic1)
import Prelude hiding (fail)

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

instance Applicative m => Alternative (AbortC m) where
  empty = AbortC (pure Nothing)
  AbortC a <|> AbortC b = AbortC (liftA2 (<|>) a b)

instance Monad m => Monad (AbortC m) where
  AbortC a >>= f = AbortC (a >>= maybe (pure Nothing) (runAbortC . f))

instance MonadFix m => MonadFix (AbortC m) where
  mfix f = AbortC (mfix (runAbort . maybe (error "mfix (AbortC): function returned failure") f))

instance MonadFail m => MonadFail (AbortC m) where
  fail = lift . fail

instance MonadTrans AbortC where
  lift = AbortC . fmap Just

instance (Carrier sig m, Effect sig) => Carrier (Abort :+: sig) (AbortC m) where
  eff (L Abort) = AbortC (pure Nothing)
  eff (R other) = AbortC (eff (handle (Just ()) (maybe (pure Nothing) runAbortC) other))
