{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving #-}
module Control.Carrier.Suspend
( runSuspend
, SuspendC(..)
, SomeEffect(..)
) where

import Control.Applicative (Alternative)
import Control.Carrier.Error.Either
import Control.Monad (MonadPlus)
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

runSuspend :: SuspendC eff m a -> m (Either (SomeEffect (eff m)) a)
runSuspend = runError . runSuspendC

newtype SuspendC eff m a = SuspendC { runSuspendC :: ErrorC (SomeEffect (eff m)) m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans (SuspendC eff) where
  lift m = SuspendC (lift m)

data SomeEffect eff
  = forall a . SomeEffect (eff a)
