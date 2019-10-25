{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies, TypeOperators, UndecidableInstances #-}

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

import Control.Algebra
import Control.Applicative (Alternative(..))
import Control.Carrier.Throw.Either
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
runFail (FailC m) = runThrow m

-- | @since 1.0.0.0
newtype FailC m a = FailC (ThrowC String m a)
  deriving (Alternative, Applicative, Functor, Monad, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Algebra sig m, CanThread sig (Either String)) => Fail.MonadFail (FailC m) where
  fail = send . Fail
  {-# INLINE fail #-}

instance (Algebra sig m, CanThread sig (Either String)) => Algebra (Fail :+: sig) (FailC m) where
  type Context (FailC m) = Either String
  -- NB: 'send' (& thus 'handleCoercible') can’t send sums, so we decompose the sum manually.
  alg (L op) = FailC (handleCoercible op)
  alg (R op) = FailC (handleCoercible op)
  {-# INLINE alg #-}
