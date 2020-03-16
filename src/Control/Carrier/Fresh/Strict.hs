{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A carrier for a 'Fresh' effect, providing access to a monotonically increasing stream of 'Int' values.
--
-- @since 1.0.0.0
module Control.Carrier.Fresh.Strict
( -- * Fresh carrier
  runFresh
, evalFresh
, FreshC(FreshC)
  -- * Fresh effect
, module Control.Effect.Fresh
) where

import Control.Algebra
import Control.Applicative (Alternative)
import Control.Carrier.State.Strict
import Control.Effect.Fresh
import Control.Monad (MonadPlus)
import Control.Monad.Fail as Fail
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
runFresh n (FreshC m) = runState n m
{-# INLINE runFresh #-}

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
evalFresh n (FreshC m) = evalState n m
{-# INLINE evalFresh #-}

-- | @since 1.0.0.0
newtype FreshC m a = FreshC { runFreshC :: StateC Int m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance Algebra sig m => Algebra (Fresh :+: sig) (FreshC m) where
  alg hdl sig ctx = FreshC $ case sig of
    L Fresh -> state $ \ i -> (i + 1, i <$ ctx)
    R other -> alg (runFreshC . hdl) (R other) ctx
  {-# INLINE alg #-}
