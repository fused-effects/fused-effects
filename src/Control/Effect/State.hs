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

import Control.Effect.Carrier
import Control.Effect.Sum
import Control.Effect.Internal
import Data.Coerce

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
get = send (Get ret)

-- | Project a function out of the current state value.
--
--   prop> snd (run (runState a (gets (applyFun f)))) == applyFun f a
gets :: (Member (State s) sig, Carrier sig m) => (s -> a) -> m a
gets f = send (Get (ret . f))

-- | Replace the state value with a new value.
--
--   prop> fst (run (runState a (put b))) == b
--   prop> snd (run (runState a (get <* put b))) == a
--   prop> snd (run (runState a (put b *> get))) == b
put :: (Member (State s) sig, Carrier sig m) => s -> m ()
put s = send (Put s (ret ()))

-- | Replace the state value with the result of applying a function to the current state value.
--   This is strict in the new state; if you need laziness, use @get >>= put . f@.
--
--   prop> fst (run (runState a (modify (+1)))) == (1 + a :: Integer)
modify :: (Member (State s) sig, Carrier sig m, Monad m) => (s -> s) -> m ()
modify f = do
   a <- get
   put $! f a

-- | Run a 'State' effect starting from the passed value.
--
--   prop> run (runState a (pure b)) == (a, b)
runState :: forall s sig m a . (Carrier sig m, Effect sig) => s -> Eff (StateC s m) a -> m (s, a)
runState s m = runStateC (interpret m) s

-- | Run a 'State' effect, yielding the result value and discarding the final state.
--
--   prop> run (evalState a (pure b)) == b
evalState :: forall s sig m a . (Carrier sig m, Effect sig, Functor m) => s -> Eff (StateC s m) a -> m a
evalState s m = fmap snd (runStateC (interpret m) s)

-- | Run a 'State' effect, yielding the final state and discarding the return value.
--
--   prop> run (execState a (pure b)) == a
execState :: forall s sig m a . (Carrier sig m, Effect sig, Functor m) => s -> Eff (StateC s m) a -> m s
execState s m = fmap fst (runStateC (interpret m) s)


newtype StateC s m a = StateC { runStateC :: s -> m (s, a) }
  deriving (Functor)

instance Monad m => Applicative (StateC s m) where
  pure a = StateC (\ s -> pure (s, a))
  StateC f <*> StateC a = StateC $ \ s -> do
    (s', f') <- f s
    (s'', a') <- a s'
    let fa = f' a'
    fa `seq` pure (s'', fa)

instance (Carrier sig m, Effect sig) => Carrier (State s :+: sig) (StateC s m) where
  ret a = StateC (\ s -> ret (s, a))
  eff op = StateC (\ s -> handleSum (eff . handleState s runStateC) (\case
    Get   k -> runStateC (k s) s
    Put s k -> runStateC  k    s) op)


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
