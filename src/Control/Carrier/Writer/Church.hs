{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Writer.Church
( -- * Writer carrier
  WriterC(..)
  -- * Writer effect
, module Control.Effect.Writer
) where

import Control.Applicative (Alternative)
import Control.Carrier.State.Church
import Control.Effect.Writer
import Control.Monad (MonadPlus)
import Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

newtype WriterC w m a = WriterC (StateC w m a)
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)
