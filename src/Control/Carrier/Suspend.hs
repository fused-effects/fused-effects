{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Suspend
( runSuspend
, SuspendC(..)
, SomeEffect(..)
) where

import Control.Applicative (Alternative)
import Control.Carrier
import Control.Carrier.Error.Either
import Control.Monad (MonadPlus)
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (throwE)

runSuspend :: SuspendC eff m a -> m (Either (SomeEffect (eff (SuspendC eff m))) a)
runSuspend = runError . runSuspendC

newtype SuspendC eff m a = SuspendC { runSuspendC :: ErrorC (SomeEffect (eff (SuspendC eff m))) m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans (SuspendC eff) where
  lift m = SuspendC (lift m)

data SomeEffect eff
  = forall a . SomeEffect (eff a)

instance (Carrier sig m, Effect sig, HFunctor eff) => Carrier (eff :+: sig) (SuspendC eff m) where
  eff (L op) = SuspendC (ErrorC (throwE (SomeEffect op)))
  eff (R op) = SuspendC (eff (R (handleCoercible op)))
