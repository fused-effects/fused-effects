{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Fail.Either
( -- * Fail effect
  module Control.Effect.Fail
  -- * Fail carrier
, runFail
, FailC(..)
  -- * Re-exports
, Has
, run
) where

import Control.Applicative (Alternative(..))
import Control.Carrier
import Control.Carrier.Error.Either
import Control.Effect.Fail
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'Fail' effect, returning failure messages in 'Left' and successful computations’ results in 'Right'.
--
--   prop> run (runFail (pure a)) === Right a
runFail :: FailC m a -> m (Either String a)
runFail = runError . runFailC

newtype FailC m a = FailC { runFailC :: ErrorC String m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Carrier sig m, Effect sig) => Fail.MonadFail (FailC m) where
  fail = send . Fail
  {-# INLINE fail #-}

instance (Carrier sig m, Effect sig) => Carrier (Fail :+: sig) (FailC m) where
  eff (L (Fail s)) = FailC (throwError s)
  eff (R other)    = FailC (eff (R (handleCoercible other)))
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
