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
import Control.Applicative
import Control.Effect.State
import Control.Monad (MonadPlus)
import Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'State' effect starting from the passed value, applying a continuation to the final state and result.
--
-- @
-- 'runState' k s ('pure' a) = k s a
-- @
-- @
-- 'runState' k s 'get' = k s s
-- @
-- @
-- 'runState' k s ('put' t) = k t ()
-- @
--
-- @since 1.1.0.0
runState :: forall s m a b . (s -> a -> m b) -> s -> StateC s m a -> m b
runState f s (StateC m) = m f s
{-# INLINE runState #-}

-- | Run a 'State' effect, yielding the result value and discarding the final state.
--
-- @
-- 'evalState' = 'runState' ('const' 'pure')
-- @
--
-- @since 1.1.0.0
evalState :: forall s m a . Applicative m => s -> StateC s m a -> m a
evalState = runState (const pure)
{-# INLINE evalState #-}

-- | Run a 'State' effect, yielding the final state and discarding the return value.
--
-- @
-- 'execState' = 'runState' ('const' '.' 'pure')
-- @
--
-- @since 1.1.0.0
execState :: forall s m a . Applicative m => s -> StateC s m a -> m s
execState = runState (const . pure)
{-# INLINE execState #-}

-- | @since 1.1.0.0
newtype StateC s m a = StateC (forall r . (s -> a -> m r) -> s -> m r)
  deriving (Functor)

instance Applicative (StateC s m) where
  pure a = StateC $ \ k s -> k s a
  {-# INLINE pure #-}

  StateC f <*> StateC a = StateC $ \ k -> f (\ s f' -> a (\ s' -> k s' . f') s)
  {-# INLINE (<*>) #-}

  liftA2 f (StateC a) (StateC b) = StateC $ \ k ->
    a (\ s' a' -> b (\ s'' -> k s'' . f a') s')
  {-# INLINE liftA2 #-}

  StateC a *> StateC b = StateC $ \ k -> a (const . b k)
  {-# INLINE (*>) #-}

  StateC a <* StateC b = StateC $ \ k ->
    a (\ s' a' -> b (\ s'' _ -> k s'' a') s')
  {-# INLINE (<*) #-}

instance Alternative m => Alternative (StateC s m) where
  empty = StateC $ \ _ _ -> empty
  {-# INLINE empty #-}

  StateC l <|> StateC r = StateC $ \ k s -> l k s <|> r k s
  {-# INLINE (<|>) #-}

instance Monad (StateC s m) where
  StateC a >>= f = StateC $ \ k -> a (\ s -> runState k s . f)
  {-# INLINE (>>=) #-}

instance Fail.MonadFail m => Fail.MonadFail (StateC s m) where
  fail = lift . Fail.fail
  {-# INLINE fail #-}

instance MonadFix m => MonadFix (StateC s m) where
  mfix f = StateC $ \ k s -> mfix (runState (curry pure) s . f . snd) >>= uncurry k
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (StateC s m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance (Alternative m, Monad m) => MonadPlus (StateC s m)

instance MonadTrans (StateC s) where
  lift m = StateC $ \ k s -> m >>= k s
  {-# INLINE lift #-}

instance Algebra sig m => Algebra (State s :+: sig) (StateC s m) where
  alg hdl sig ctx = StateC $ \ k s -> case sig of
    L Get     -> k s (s <$ ctx)
    L (Put s) -> k s       ctx
    R other   -> thread (uncurry (runState (curry pure)) ~<~ hdl) other (s, ctx) >>= uncurry k
  {-# INLINE alg #-}
