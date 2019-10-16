{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

-- | A carrier for a 'Fresh' effect, providing access to a monotonically increasing stream of 'Int' values.
module Control.Carrier.Fresh.Strict
( -- * Fresh carrier
  runFresh
, FreshC(..)
  -- * Fresh effect
, module Control.Effect.Fresh
) where

import Control.Applicative (Alternative(..))
import Control.Carrier
import Control.Carrier.State.Strict
import Control.Effect.Fresh
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'Fresh' effect counting up from 0.
--
-- @
-- 'runFresh' ('pure' a) = 'pure' a
-- @
-- @
-- 'runFresh' 'fresh' = 'pure' 0
-- @
--
-- @since 0.1.0.0
runFresh :: Functor m => FreshC m a -> m a
runFresh = evalState 0 . runFreshC

-- | @since 1.0.0.0
newtype FreshC m a = FreshC { runFreshC :: StateC Int m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Carrier sig m, Effect sig) => Carrier (Fresh :+: sig) (FreshC m) where
  eff (L (Fresh   k)) = FreshC $ do
    i <- get
    put (succ i)
    runFreshC (k i)
  eff (R other)       = FreshC (eff (R (handleCoercible other)))
  {-# INLINE eff #-}
