{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Fresh.Church
( -- * Fresh carrier
  FreshC(FreshC)
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

newtype FreshC m a = FreshC { runFreshC :: StateC Int m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance Algebra sig m => Algebra (Fresh :+: sig) (FreshC m) where
  alg hdl sig ctx = FreshC $ case sig of
    L Fresh -> gets (<$ ctx) <* modify (+ (1 :: Int))
    R other -> alg (runFreshC . hdl) (R other) ctx
  {-# INLINE alg #-}
