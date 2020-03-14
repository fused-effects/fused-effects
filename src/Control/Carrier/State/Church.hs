{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | A church-encoded carrier for the 'State' effect.

Note that the parameter order in 'runState', 'evalState', and 'execState' is reversed compared the equivalent functions provided by @transformers@. This is an intentional decision made to enable the composition of effect handlers with '.' without invoking 'flip'.

@since 1.1.0.0
-}
module Control.Carrier.State.Church
( -- * State carrier
  runState
, evalState
, execState
, StateC(StateC)
  -- * State effect
, module Control.Effect.State
) where

import Control.Algebra
import Control.Applicative (Alternative(..))
import Control.Effect.State
import Control.Monad (MonadPlus)
import Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'State' effect starting from the passed value.
--
-- @
-- 'runState' s ('pure' a) = 'pure' (s, a)
-- @
-- @
-- 'runState' s 'get' = 'pure' (s, s)
-- @
-- @
-- 'runState' s ('put' t) = 'pure' (t, ())
-- @
--
-- @since 1.1.0.0
runState :: forall s m a . Applicative m => s -> StateC s m a -> m (s, a)
runState s (StateC m) = m (flip (curry pure)) s
{-# INLINE runState #-}

-- | Run a 'State' effect, yielding the result value and discarding the final state.
--
-- @
-- 'evalState' s m = 'fmap' 'snd' ('runState' s m)
-- @
--
-- @since 1.1.0.0
evalState :: forall s m a . Applicative m => s -> StateC s m a -> m a
evalState s (StateC m) = m (const . pure) s
{-# INLINE evalState #-}

-- | Run a 'State' effect, yielding the final state and discarding the return value.
--
-- @
-- 'execState' s m = 'fmap' 'fst' ('runState' s m)
-- @
--
-- @since 1.1.0.0
execState :: forall s m a . Applicative m => s -> StateC s m a -> m s
execState s (StateC m) = m (const pure) s
{-# INLINE execState #-}

-- | @since 1.1.0.0
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

instance Algebra sig m => Algebra (State s :+: sig) (StateC s m) where
  alg hdl sig ctx = StateC $ \ k s -> case sig of
    L Get     -> k (s <$ ctx) s
    L (Put s) -> k       ctx  s
    R other   -> thread (uncurry runState ~<~ hdl) other (s, ctx) >>= uncurry (flip k)
  {-# INLINE alg #-}
