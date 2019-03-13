{-# LANGUAGE DeriveFunctor, ExplicitForAll, FlexibleContexts, FlexibleInstances, KindSignatures, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Effect.State.Lazy
( State (..)
, get
, gets
, put
, modify
, modifyLazy
, runLazyState
, evalLazyState
, execLazyState
, LazyStateC(..)
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Control.Effect.Sum
import Control.Effect.State
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Prelude hiding (fail)

-- | Replace the state value with the result of applying a function to the current state value.
--   This is lazy in the new state; injudicious use of this function may lead to space leaks.
modifyLazy :: (Member (State s) sig, Carrier sig m) => (s -> s) -> m ()
modifyLazy f = get >>= put . f

newtype LazyStateC s m a = LazyStateC { runLazyStateC :: s -> m (s, a) }

instance Functor m => Functor (LazyStateC s m) where
  fmap f m = LazyStateC $ \ s -> fmap (\ ~(s', a) -> (s', f a)) $ runLazyStateC m s
  {-# INLINE fmap #-}

instance (Functor m, Monad m) => Applicative (LazyStateC s m) where
  pure a = LazyStateC $ \ s -> pure (s, a)
  {-# INLINE pure #-}
  LazyStateC mf <*> LazyStateC mx = LazyStateC $ \ s -> do
    ~(s', f) <- mf s
    ~(s'', x) <- mx s'
    return (s'', f x)
  {-# INLINE (<*>) #-}

instance Monad m => Monad (LazyStateC s m) where
  m >>= k  = LazyStateC $ \ s -> do
    ~(s', a) <- runLazyStateC m s
    runLazyStateC (k a) s'
  {-# INLINE (>>=) #-}

instance (Alternative m, Monad m) => Alternative (LazyStateC s m) where
  empty = LazyStateC (const empty)
  {-# INLINE empty #-}
  LazyStateC l <|> LazyStateC r = LazyStateC (\ s -> l s <|> r s)
  {-# INLINE (<|>) #-}

instance MonadFail m => MonadFail (LazyStateC s m) where
  fail s = LazyStateC (const (fail s))
  {-# INLINE fail #-}

instance MonadIO m => MonadIO (LazyStateC s m) where
  liftIO io = LazyStateC (\ s -> (,) s <$> liftIO io)
  {-# INLINE liftIO #-}

instance (Alternative m, Monad m) => MonadPlus (LazyStateC s m)

instance MonadTrans (LazyStateC s) where
  lift m = LazyStateC (\ s -> (,) s <$> m)
  {-# INLINE lift #-}

instance (Carrier sig m, Effect sig) => Carrier (State s :+: sig) (LazyStateC s m) where
  eff (L (Get   k)) = LazyStateC (\ s -> runLazyState s (k s))
  eff (L (Put s k)) = LazyStateC (\ _ -> runLazyState s k)
  eff (R other)     = LazyStateC (\ s -> eff (handle (s, ()) (uncurry runLazyState) other))
  {-# INLINE eff #-}

-- | Run a lazy 'State' effect, yielding the result value and the final state.
--   More programs terminate with lazy state than strict state, but injudicious
--   use of lazy state may lead to thunk buildup.
--
--   prop> run (runLazyState a (pure b)) == (a, b)
--   prop> take 5 . snd . run $ runLazyState () (traverse pure [1..]) == [1,2,3,4,5]
runLazyState :: s -> LazyStateC s m a -> m (s, a)
runLazyState = flip runLazyStateC

-- | Run a lazy 'State' effect, yielding the result value and discarding the final state.
--
--   prop> run (evalLazyState a (pure b)) == b
evalLazyState :: forall s m a . Functor m => s -> LazyStateC s m a -> m a
evalLazyState s = fmap snd . runLazyState s

-- | Run a lazy 'State' effect, yielding the final state and discarding the return value.
--
--   prop> run (execLazyState a (pure b)) == a
execLazyState :: forall s m a . Functor m => s -> LazyStateC s m a -> m s
execLazyState s = fmap fst . runLazyState s

-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
