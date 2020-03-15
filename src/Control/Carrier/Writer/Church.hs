{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Writer.Church
( -- * Writer carrier
  runWriter
, WriterC(WriterC)
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

runWriter :: Monoid w => (w -> a -> m b) -> WriterC w m a -> m b
runWriter k = runState k mempty . runWriterC
{-# INLINE runWriter #-}

newtype WriterC w m a = WriterC (StateC w m a)
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)
