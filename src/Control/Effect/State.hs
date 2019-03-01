{-# LANGUAGE DeriveFunctor, ExplicitForAll, FlexibleContexts, FlexibleInstances, KindSignatures, LambdaCase, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Effect.State
( State(..)
, get
, gets
, put
, modify
, runState
, evalState
, execState
, StateC(..)
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Control.Effect.Sum
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Coerce
import Prelude hiding (fail)

data State s (m :: * -> *) k
  = Get (s -> k)
  | Put s k
  deriving (Functor)

instance HFunctor (State s) where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect (State s) where
  handle state handler (Get k)   = Get   (handler . (<$ state) . k)
  handle state handler (Put s k) = Put s (handler . (<$ state) $ k)

-- | Get the current state value.
--
--   prop> snd (run (runState a get)) == a
get :: (Member (State s) sig, Carrier sig m) => m s
get = send (Get pure)

-- | Project a function out of the current state value.
--
--   prop> snd (run (runState a (gets (applyFun f)))) == applyFun f a
gets :: (Member (State s) sig, Carrier sig m) => (s -> a) -> m a
gets f = send (Get (pure . f))

-- | Replace the state value with a new value.
--
--   prop> fst (run (runState a (put b))) == b
--   prop> snd (run (runState a (get <* put b))) == a
--   prop> snd (run (runState a (put b *> get))) == b
put :: (Member (State s) sig, Carrier sig m) => s -> m ()
put s = send (Put s (pure ()))

-- | Replace the state value with the result of applying a function to the current state value.
--   This is strict in the new state; if you need laziness, use @get >>= put . f@.
--
--   prop> fst (run (runState a (modify (+1)))) == (1 + a :: Integer)
modify :: (Member (State s) sig, Carrier sig m) => (s -> s) -> m ()
modify f = do
  a <- get
  put $! f a

-- | Run a 'State' effect starting from the passed value.
--
--   prop> run (runState a (pure b)) == (a, b)
runState :: s -> StateC s m a -> m (s, a)
runState = flip runStateC

-- | Run a 'State' effect, yielding the result value and discarding the final state.
--
--   prop> run (evalState a (pure b)) == b
evalState :: forall s m a . Functor m => s -> StateC s m a -> m a
evalState s = fmap snd . runState s

-- | Run a 'State' effect, yielding the final state and discarding the return value.
--
--   prop> run (execState a (pure b)) == a
execState :: forall s m a . Functor m => s -> StateC s m a -> m s
execState s = fmap fst . runState s


newtype StateC s m a = StateC { runStateC :: s -> m (s, a) }
  deriving (Functor)

instance Monad m => Applicative (StateC s m) where
  pure a = StateC (\ s -> pure (s, a))
  StateC f <*> StateC a = StateC $ \ s -> do
    (s', f') <- f s
    (s'', a') <- a s'
    let fa = f' a'
    fa `seq` pure (s'', fa)

instance (Alternative m, Monad m) => Alternative (StateC s m) where
  empty = StateC (const empty)
  StateC l <|> StateC r = StateC (\ s -> l s <|> r s)

instance Monad m => Monad (StateC s m) where
  StateC m >>= f = StateC $ \ s -> do
    (s', a) <- m s
    let fa = f a
    fa `seq` runState s' fa

instance MonadFail m => MonadFail (StateC s m) where
  fail s = StateC (const (fail s))

instance MonadIO m => MonadIO (StateC s m) where
  liftIO io = StateC (\ s -> (,) s <$> liftIO io)

instance (Alternative m, Monad m) => MonadPlus (StateC s m)

instance MonadTrans (StateC s) where
  lift m = StateC (\ s -> (,) s <$> m)

instance (Carrier sig m, Effect sig) => Carrier (State s :+: sig) (StateC s m) where
  eff op = StateC (\ s -> handleSum (eff . handle (s, ()) (uncurry runState)) (\case
    Get   k -> runState s (k s)
    Put s k -> runState s k) op)


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
