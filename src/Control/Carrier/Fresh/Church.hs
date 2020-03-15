{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Fresh.Church
( -- * Fresh carrier
  FreshC(..)
  -- * Fresh effect
, module Control.Effect.Fresh
) where

import Control.Applicative (Alternative)
import Control.Carrier.State.Church
import Control.Effect.Fresh
import Control.Monad (MonadPlus)
import Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

newtype FreshC m a = FreshC (StateC Int m a)
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)
