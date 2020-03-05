{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
module Control.Carrier.State.Church
( -- * State carrier
  runState
, StateC(StateC)
  -- * State effect
, module Control.Effect.State
) where

import Control.Applicative (Alternative(..))
import Control.Effect.State
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail as Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

runState :: Applicative m => s -> StateC s m a -> m (s, a)
runState s (StateC m) = m (\ a s -> pure (s, a)) s

runStateK :: (a -> s -> m r) -> StateC s m a -> s -> m r
runStateK k (StateC m) = m k

newtype StateC s m a = StateC (forall r . (a -> s -> m r) -> s -> m r)
  deriving (Functor)

instance Applicative (StateC s m) where
  pure a = StateC $ \ k s -> k a s

  StateC f <*> StateC a = StateC $ \ k -> f (a . (k .))

instance Alternative m => Alternative (StateC s m) where
  empty = StateC $ \ _ _ -> empty
  StateC l <|> StateC r = StateC $ \ k s -> l k s <|> r k s

instance Monad (StateC s m) where
  StateC a >>= f = StateC $ \ k -> a (runStateK k . f)

instance Fail.MonadFail m => Fail.MonadFail (StateC s m) where
  fail = lift . Fail.fail

instance MonadIO m => MonadIO (StateC s m) where
  liftIO = lift . liftIO

instance (Alternative m, Monad m) => MonadPlus (StateC s m)

instance MonadTrans (StateC s) where
  lift m = StateC $ \ k s -> m >>= flip k s
