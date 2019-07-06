{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric, DerivingStrategies, FlexibleInstances, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Effect.Fail
( Fail(..)
, MonadFail(..)
, runFail
, FailC(..)
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Control.Effect.Error
import Control.Effect.Sum
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import GHC.Generics (Generic1)
import Prelude hiding (fail)

newtype Fail (m :: * -> *) k = Fail String
  deriving stock (Functor, Generic1)
  deriving anyclass (HFunctor, Effect)

-- | Run a 'Fail' effect, returning failure messages in 'Left' and successful computations’ results in 'Right'.
--
--   prop> run (runFail (pure a)) === Right a
runFail :: FailC m a -> m (Either String a)
runFail = runError . runFailC

newtype FailC m a = FailC { runFailC :: ErrorC String m a }
  deriving newtype (Alternative, Applicative, Functor, Monad, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Carrier sig m, Effect sig) => MonadFail (FailC m) where
  fail s = FailC (throwError s)
  {-# INLINE fail #-}

instance (Carrier sig m, Effect sig) => Carrier (Fail :+: sig) (FailC m) where
  eff (L (Fail s)) = fail s
  eff (R other)    = FailC (eff (R (handleCoercible other)))
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
