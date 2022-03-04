{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | A carrier for the 'State' effect. It uses an 'IORef' internally to handle its state, and thus admits a 'MonadUnliftIO' instance. Because the state operations are performed impurely, this carrier will not lose state effects even with nefarious uses of 'liftWith'.

Unlike the other carriers for 'State', this carrier's effects will not backtrack when run in conjuction with 'NonDet' effects.

@since 1.1.2.0
-}
module Control.Carrier.State.IORef
( -- * Impure state carrier
  runState
, runStateRef
, evalState
, execState
, StateC(..)
-- * State effect
, module Control.Effect.State
) where

import           Control.Algebra
import           Control.Applicative (Alternative(..))
import           Control.Carrier.Reader
import           Control.Effect.State
import           Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class
import           Data.IORef

-- | Run a 'State' effect starting from the passed value.
--
--   prop> run (runState a (pure b)) === (a, b)
--
-- @since 1.1.2.0
runState :: MonadIO m => s -> StateC s m a -> m (s, a)
runState s x = do
  ref <- liftIO $ newIORef s
  result <- runReader ref . runStateC $ x
  final <- liftIO . readIORef $ ref
  pure (final, result)
{-# INLINE[3] runState #-}

-- | Run a 'State' effect starting from the passed 'IORef'.
--
--   prop> run (runState a (pure b)) === (a, b)
--
-- @since 1.1.2.0
runStateRef :: MonadIO m => IORef s -> StateC s m a -> m (s, a)
runStateRef ref x = do
  result <- runReader ref . runStateC $ x
  final <- liftIO . readIORef $ ref
  pure (final, result)
{-# INLINE[3] runStateRef #-}

-- | Run a 'State' effect, yielding the result value and discarding the final state.
--
--   prop> run (evalState a (pure b)) === b
--
-- @since 1.1.2.0
evalState :: forall s m a . MonadIO m => s -> StateC s m a -> m a
evalState s x = do
  ref <- liftIO $ newIORef s
  runReader ref . runStateC $ x
{-# INLINE[3] evalState #-}

-- | Run a 'State' effect, yielding the final state and discarding the return value.
--
--   prop> run (execState a (pure b)) === a
--
-- @since 1.1.2.0
execState :: forall s m a . MonadIO m => s -> StateC s m a -> m s
execState s = fmap fst . runState s
{-# INLINE[3] execState #-}

-- | @since 1.1.2.0
newtype StateC s m a = StateC { runStateC :: ReaderC (IORef s) m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans, MonadUnliftIO)

instance (MonadIO m, Algebra sig m) => Algebra (State s :+: sig) (StateC s m) where
  alg hdl sig ctx = case sig of
    L act -> do
      ref <- StateC (ask @(IORef s))
      (<$ ctx) <$> case act of
        Put s -> liftIO (writeIORef ref s)
        Get   -> liftIO (readIORef ref)
    R other -> StateC (alg (runStateC . hdl) (R other) ctx)
  {-# INLINE alg #-}
