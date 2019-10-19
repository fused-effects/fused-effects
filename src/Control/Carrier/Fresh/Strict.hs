{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

-- | A carrier for a 'Fresh' effect, providing access to a monotonically increasing stream of 'Int' values.
module Control.Carrier.Fresh.Strict
( -- * Fresh carrier
  runFresh
, evalFresh
, FreshC(FreshC)
  -- * Fresh effect
, module X
) where

import Control.Applicative (Alternative(..))
import Control.Carrier
import Control.Carrier.State.Strict
import Control.Effect.Fresh
import Control.Effect.Fresh as X (Fresh)
import Control.Effect.Fresh as X hiding (Fresh)
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'Fresh' effect counting up from 0.
--
-- @
-- 'runFresh' n ('pure' a) = 'pure' (n, a)
-- @
-- @
-- 'runFresh' n 'fresh' = 'pure' (n '+' 1, n)
-- @
--
-- @since 0.1.0.0
runFresh :: Int -> FreshC m a -> m (Int, a)
runFresh n = runState n . runFreshC

-- | Run a 'Fresh' effect counting up from an initial value, and forgetting the final value.
--
-- @
-- 'evalFresh' n ('pure' a) = 'pure' a
-- @
-- @
-- 'evalFresh' n 'fresh' = 'pure' n
-- @
--
-- @since 1.0.0.0
evalFresh :: Functor m => Int -> FreshC m a -> m a
evalFresh n = evalState n . runFreshC

-- | @since 1.0.0.0
newtype FreshC m a = FreshC { runFreshC :: StateC Int m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Carrier sig m, Effect sig) => Carrier (Fresh :+: sig) (FreshC m) where
  eff (L (Fresh k)) = FreshC (get <* modify (+ (1 :: Int))) >>= k
  eff (R other)     = FreshC (eff (R (handleCoercible other)))
  {-# INLINE eff #-}
