{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A carrier for a 'Control.Effect.Fail.Fail' effect, returning the result as an 'Either' 'String'. Failed computations will return a 'Left' containing the 'String' value passed to 'Fail.fail'.
--
-- @since 1.0.0.0
module Control.Carrier.Fail.Either
( -- * Fail carrier
  runFail
, FailC(..)
  -- * Fail effect
, module Control.Effect.Fail
) where

import Control.Algebra
import Control.Applicative (Alternative(..))
import Control.Carrier.Throw.Either
import Control.Effect.Fail
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'Control.Effect.Fail.Fail' effect, returning failure messages in 'Left' and successful computations’ results in 'Right'.
--
-- @
-- 'runFail' ('pure' a) = 'pure' ('Right' a)
-- @
-- @
-- 'runFail' ('Fail.fail' s) = 'pure' ('Left' s)
-- @
--
-- @since 1.0.0.0
runFail :: FailC m a -> m (Either String a)
runFail (FailC m) = runThrow m
{-# INLINE runFail #-}

-- | @since 1.0.0.0
newtype FailC m a = FailC (ThrowC String m a)
  deriving (Algebra (Fail :+: sig), Alternative, Applicative, Functor, Monad, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance Algebra sig m => Fail.MonadFail (FailC m) where
  fail = send . Fail
  {-# INLINE fail #-}
