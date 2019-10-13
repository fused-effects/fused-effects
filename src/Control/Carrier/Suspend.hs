{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving #-}
module Control.Carrier.Suspend
( SuspendC(..)
, SomeEffect(..)
) where

import Control.Applicative (Alternative)
import Control.Carrier.Error.Either
import Control.Monad (MonadPlus)
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class

newtype SuspendC eff m a = SuspendC { runSuspendC :: ErrorC (SomeEffect (eff m)) m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

data SomeEffect eff
  = forall a . SomeError (eff a)
