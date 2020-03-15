{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Fresh.Church
( -- * Fresh carrier
  runFresh
, evalFresh
, FreshC(FreshC)
  -- * Fresh effect
, module Control.Effect.Fresh
) where

import Control.Algebra
import Control.Applicative (Alternative)
import Control.Carrier.State.Church
import Control.Effect.Fresh
import Control.Monad (MonadPlus)
import Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'Fresh' effect counting up from 0.
--
-- @
-- 'runFresh' k n ('pure' a) = k n a
-- @
-- @
-- 'runFresh' k n 'fresh' = k (n '+' 1) n
-- @
--
-- @since 1.1.0.0
runFresh :: (Int -> a -> m b) -> Int -> FreshC m a -> m b
runFresh k n = runState k n . runFreshC
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
-- @since 1.1.0.0
evalFresh :: Applicative m => Int -> FreshC m a -> m a
evalFresh n = evalState n . runFreshC
{-# INLINE evalFresh #-}

-- | @since 1.1.0.0
newtype FreshC m a = FreshC { runFreshC :: StateC Int m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance Algebra sig m => Algebra (Fresh :+: sig) (FreshC m) where
  alg hdl sig ctx = FreshC $ case sig of
    L Fresh -> gets (<$ ctx) <* modify (+ (1 :: Int))
    R other -> alg (runFreshC . hdl) (R other) ctx
  {-# INLINE alg #-}
