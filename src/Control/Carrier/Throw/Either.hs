{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Control.Carrier.Throw.Either
( -- * Throw effect
  module Control.Effect.Throw
  -- * Throw carrier
, runThrow
, ThrowC(..)
  -- * Re-exports
, Carrier
, run
) where

import Control.Applicative (Alternative)
import Control.Carrier
import Control.Carrier.Error.Either
import Control.Effect.Throw
import Control.Monad (MonadPlus)
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

runThrow :: ThrowC e m a -> m (Either e a)
runThrow = runError . runThrowC

newtype ThrowC e m a = ThrowC { runThrowC :: ErrorC e m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)
