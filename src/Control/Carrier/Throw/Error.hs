{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Throw.Error
( -- * Throw effect
  module Control.Effect.Throw
  -- * Throw carrier
, runThrow
, ThrowC(ThrowC)
  -- * Re-exports
, Carrier
, run
) where

import Control.Applicative (Alternative)
import Control.Carrier
import Control.Effect.Error
import Control.Effect.Throw
import Control.Monad (MonadPlus)
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

newtype ThrowC e m a = ThrowC { runThrow :: m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans (ThrowC e) where
  lift = ThrowC

instance (Has (Error e) sig m) => Carrier (Throw e :+: sig) (ThrowC e m) where
  eff (L (Throw e)) = ThrowC (throwError e)
  eff (R other)     = ThrowC (eff (handleCoercible other))
