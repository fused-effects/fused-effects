{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.State.Church
( -- * State carrier
  runState
, StateC(StateC)
  -- * State effect
, module Control.Effect.State
) where

import Control.Algebra
import Control.Applicative (Alternative(..))
import Control.Effect.State
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

runState :: forall s m a . Applicative m => s -> StateC s m a -> m (s, a)
runState s (StateC m) = m (\ a s -> pure (s, a)) s
{-# INLINE runState #-}

newtype StateC s m a = StateC { runStateC :: forall r . (a -> s -> m r) -> s -> m r }
  deriving (Functor)

instance Applicative (StateC s m) where
  pure a = StateC $ \ k s -> k a s
  {-# INLINE pure #-}

  StateC f <*> StateC a = StateC $ \ k -> f (a . (k .))
  {-# INLINE (<*>) #-}

instance Alternative m => Alternative (StateC s m) where
  empty = StateC $ \ _ _ -> empty
  {-# INLINE empty #-}

  StateC l <|> StateC r = StateC $ \ k s -> l k s <|> r k s
  {-# INLINE (<|>) #-}

instance Monad (StateC s m) where
  StateC a >>= f = StateC $ \ k -> a (\ a' -> runStateC (f a') k)
  {-# INLINE (>>=) #-}

instance Fail.MonadFail m => Fail.MonadFail (StateC s m) where
  fail = lift . Fail.fail
  {-# INLINE fail #-}

instance MonadFix m => MonadFix (StateC s m) where
  mfix f = StateC $ \ k s -> mfix (runState s . f . snd) >>= uncurry (flip k)
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (StateC s m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance (Alternative m, Monad m) => MonadPlus (StateC s m)

instance MonadTrans (StateC s) where
  lift m = StateC $ \ k s -> m >>= flip k s
  {-# INLINE lift #-}

instance (Algebra sig m, Effect sig) => Algebra (State s :+: sig) (StateC s m) where
  alg hom = \case
    L (Get   k) -> StateC $ \ k' s -> runState s (hom (k s)) >>= uncurry (flip k')
    L (Put s k) -> StateC $ \ k' _ -> runState s (hom k)     >>= uncurry (flip k')
    R other     -> StateC $ \ k  s -> alg id (thread (s, ()) (uncurry runState . fmap hom) other) >>= uncurry (flip k)
  {-# INLINE alg #-}
