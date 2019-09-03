{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Fail
( -- * Fail effect
  module Control.Effect.Fail
  -- * Fail carrier
, runFail
, FailC(..)
  -- * Re-exports
, Carrier
, Member
, MonadFail(..)
, run
) where

import Control.Applicative (Alternative(..))
import Control.Carrier.Class
import Control.Carrier.Error.Either
import qualified Control.Effect.Fail as Fail
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'Fail' effect, returning failure messages in 'Left' and successful computations’ results in 'Right'.
--
--   prop> run (runFail (pure a)) === Right a
runFail :: FailC m a -> m (Either String a)
runFail = runError . runFailC

newtype FailC m a = FailC { runFailC :: ErrorC String m a }
  deriving newtype (Alternative, Applicative, Functor, Monad, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Carrier sig m, Effect sig) => Fail.MonadFail (FailC m) where
  fail s = FailC (throwError s)
  {-# INLINE fail #-}

instance (Carrier sig m, Effect sig) => Carrier (Fail :+: sig) (FailC m) where
  eff (L (Fail s)) = Fail.fail s
  eff (R other)    = FailC (eff (R (handleCoercible other)))
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
