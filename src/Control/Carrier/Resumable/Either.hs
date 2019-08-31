{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Resumable.Either
( -- * Resumable effect
  module Control.Effect.Resumable
  -- * Resumable carrier
, runResumable
, ResumableC(..)
) where

import Control.Applicative (Alternative(..))
import Control.Carrier.Class
import Control.Carrier.Error
import Control.Effect.Resumable
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'Resumable' effect, returning uncaught errors in 'Left' and successful computations’ values in 'Right'.
--
--   prop> run (runResumable (pure a)) === Right @(SomeError Identity) @Int a
runResumable :: ResumableC err m a -> m (Either (SomeError err) a)
runResumable = runError . runResumableC

newtype ResumableC err m a = ResumableC { runResumableC :: ErrorC (SomeError err) m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Carrier sig m, Effect sig) => Carrier (Resumable err :+: sig) (ResumableC err m) where
  eff (L (Resumable err _)) = ResumableC (throwError (SomeError err))
  eff (R other)             = ResumableC (eff (R (handleCoercible other)))
  {-# INLINE eff #-}
