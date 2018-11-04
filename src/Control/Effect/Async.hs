{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, GADTs,
             MultiParamTypeClasses, RankNTypes, StandaloneDeriving, TypeOperators, UndecidableInstances, LambdaCase #-}
module Control.Effect.Async
  ( Asynchronous (..)
  , async
  , asyncBound
  , wait
  , poll
  , runAsynchronous
  , AsynchronousC (..)
  -- | Re-exports from Control.Concurrent.Async
  , Async
  , cancel
  ) where

import           Control.Concurrent.Async (Async, asyncThreadId, cancel, poll)
import qualified Control.Concurrent.Async as C
import           Control.Monad.IO.Class
import           Control.Exception (SomeException)

import Control.Effect.Carrier
import Control.Effect.Internal
import Control.Effect.Sum

data Spawn = ForkIO | ForkOS | ForkOn Int

data Asynchronous m k where
  MkAsync :: Spawn -> m a -> (Async a -> k) -> Asynchronous m k
  Wait    :: Async a -> (a -> k) -> Asynchronous m k

instance Functor (Asynchronous m) where
  fmap f (MkAsync s m k) = MkAsync s m (f . k)
  fmap f (Wait a k) = Wait a (f . k)

instance HFunctor Asynchronous where
  hmap f (MkAsync s m k) = MkAsync s (f m) k
  hmap f (Wait a k)    = Wait a k

-- | Spawn an asynchronous action in a separate thread.
-- The 'asyncWithUnmask' function is not provided due to
-- implementation details that prevent correct typing.
async :: (Member Asynchronous sig, Carrier sig m) => m a -> m (Async a)
async act = send (MkAsync ForkIO act ret)

-- | Like 'async' but using 'forkOS' internally.
asyncBound :: (Member Asynchronous sig, Carrier sig m) => m a -> m (Async a)
asyncBound act = send (MkAsync ForkOS act ret)

-- | Like 'async' but using 'forkOn' internally.
asyncOn :: (Member Asynchronous sig, Carrier sig m) => Int -> m a -> m (Async a)
asyncOn n act = send (MkAsync (ForkOn n) act ret)

-- | Wait for an asynchronous action to complete, and return its
-- value. If the asynchronous action threw an exception, then the
-- exception is re-thrown by wait.
wait :: (Member Asynchronous sig, Carrier sig m) => Async a -> m a
wait it = send (Wait it ret)

-- | Run computations asynchronously in a 'MonadIO'.
runAsynchronous :: (Carrier sig m, MonadIO m)
                => (forall x . m x -> IO x)
                -> Eff (AsynchronousC m) a
                -> m a
runAsynchronous handler = runAsynchronousC handler . interpret

newtype AsynchronousC m a = AsynchronousC ((forall x . m x -> IO x) -> m a)

runAsynchronousC :: (forall x . m x -> IO x) -> AsynchronousC m a -> m a
runAsynchronousC handler (AsynchronousC m) = m handler

instance (Carrier sig m, MonadIO m) => Carrier (Asynchronous :+: sig) (AsynchronousC m) where
  ret a = AsynchronousC (const (ret a))
  eff op = AsynchronousC (\handler -> handleSum
                                      (eff . handlePure (runAsynchronousC handler))
                                      (\case
                                          (MkAsync ForkIO act k) -> liftIO (C.async (handler (runAsynchronousC handler act))) >>= runAsynchronousC handler . k
                                          (MkAsync ForkOS act k) -> liftIO (C.asyncBound (handler (runAsynchronousC handler act))) >>= runAsynchronousC handler . k
                                          (MkAsync (ForkOn n) act k) -> liftIO (C.asyncOn n (handler (runAsynchronousC handler act))) >>= runAsynchronousC handler . k
                                          (Wait it k) -> liftIO (C.wait it) >>= runAsynchronousC handler . k
                                      )
                                      op)
