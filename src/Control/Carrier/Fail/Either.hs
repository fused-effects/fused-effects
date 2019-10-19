{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

-- | A carrier for a 'Fail' effect, returning the result as an 'Either' 'String'. Failed computations will return a 'Left' containing the 'String' value passed to 'Fail.fail'.
--
-- @since 1.0.0.0
module Control.Carrier.Fail.Either
( -- * Fail carrier
  runFail
, FailC(..)
  -- * Fail effect
, module Control.Effect.Fail
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
-- @
-- 'runFail' ('pure' a) = 'pure' ('Right' a)
-- @
-- @
-- 'runFail' ('fail' s) = 'pure' ('Left' s)
-- @
--
-- @since 1.0.0.0
runFail :: FailC m a -> m (Either String a)
runFail = runError . runFailC

-- | @since 1.0.0.0
newtype FailC m a = FailC { runFailC :: ErrorC String m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Carrier sig m, Effect sig) => Fail.MonadFail (FailC m) where
  fail = send . Throw
  {-# INLINE fail #-}

instance (Carrier sig m, Effect sig) => Carrier (Fail :+: sig) (FailC m) where
  eff (L (Throw s)) = FailC (throwError s)
  eff (R other)     = FailC (eff (R (handleCoercible other)))
  {-# INLINE eff #-}
