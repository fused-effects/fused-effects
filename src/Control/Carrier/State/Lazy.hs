{-# LANGUAGE DeriveFunctor, ExplicitForAll, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Carrier.State.Lazy
( -- * State effect
  module State
  -- * Lazy state carrier
, runState
, evalState
, execState
, StateC(..)
) where

import Control.Applicative (Alternative(..))
import Control.Carrier
import Control.Effect.State as State
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Prelude hiding (fail)

newtype StateC s m a = StateC { runStateC :: s -> m (s, a) }

instance Functor m => Functor (StateC s m) where
  fmap f m = StateC $ \ s -> fmap (\ ~(s', a) -> (s', f a)) $ runStateC m s
  {-# INLINE fmap #-}

instance (Functor m, Monad m) => Applicative (StateC s m) where
  pure a = StateC $ \ s -> pure (s, a)
  {-# INLINE pure #-}
  StateC mf <*> StateC mx = StateC $ \ s -> do
    ~(s', f) <- mf s
    ~(s'', x) <- mx s'
    return (s'', f x)
  {-# INLINE (<*>) #-}
  m *> k = m >>= \_ -> k
  {-# INLINE (*>) #-}

instance Monad m => Monad (StateC s m) where
  m >>= k  = StateC $ \ s -> do
    ~(s', a) <- runStateC m s
    runStateC (k a) s'
  {-# INLINE (>>=) #-}

instance (Alternative m, Monad m) => Alternative (StateC s m) where
  empty = StateC (const empty)
  {-# INLINE empty #-}
  StateC l <|> StateC r = StateC (\ s -> l s <|> r s)
  {-# INLINE (<|>) #-}

instance MonadFail m => MonadFail (StateC s m) where
  fail s = StateC (const (fail s))
  {-# INLINE fail #-}

instance MonadFix m => MonadFix (StateC s m) where
  mfix f = StateC (\ s -> mfix (runState s . f . snd))
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (StateC s m) where
  liftIO io = StateC (\ s -> (,) s <$> liftIO io)
  {-# INLINE liftIO #-}

instance (Alternative m, Monad m) => MonadPlus (StateC s m)

instance MonadTrans (StateC s) where
  lift m = StateC (\ s -> (,) s <$> m)
  {-# INLINE lift #-}

instance (Carrier sig m, Effect sig) => Carrier (State s :+: sig) (StateC s m) where
  eff (L (Get   k)) = StateC (\ s -> runState s (k s))
  eff (L (Put s k)) = StateC (\ _ -> runState s k)
  eff (R other)     = StateC (\ s -> eff (handle (s, ()) (uncurry runState) other))
  {-# INLINE eff #-}

-- | Run a lazy 'State' effect, yielding the result value and the final state.
--   More programs terminate with lazy state than strict state, but injudicious
--   use of lazy state may lead to thunk buildup.
--
--   prop> run (runState a (pure b)) === (a, b)
--   prop> take 5 . snd . run $ runState () (traverse pure [1..]) === [1,2,3,4,5]
runState :: s -> StateC s m a -> m (s, a)
runState s c = runStateC c s
{-# INLINE[3] runState #-}

-- | Run a lazy 'State' effect, yielding the result value and discarding the final state.
--
--   prop> run (evalState a (pure b)) === b
evalState :: forall s m a . Functor m => s -> StateC s m a -> m a
evalState s = fmap snd . runState s
{-# INLINE[3] evalState #-}

-- | Run a lazy 'State' effect, yielding the final state and discarding the return value.
--
--   prop> run (execState a (pure b)) === a
execState :: forall s m a . Functor m => s -> StateC s m a -> m s
execState s = fmap fst . runState s
{-# INLINE[3] execState #-}

-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
