{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
