{-# LANGUAGE ExistentialQuantification #-}
module Control.Carrier.Suspend
( SuspendC(..)
, SomeEffect(..)
) where

import Control.Carrier.Error.Either

newtype SuspendC eff m a = SuspendC { runSuspendC :: ErrorC (SomeEffect (eff m)) m a }

data SomeEffect eff
  = forall a . SomeError (eff a)
