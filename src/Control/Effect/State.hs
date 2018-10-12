{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Control.Effect.State
( State(..)
, get
, gets
, put
, modify
, modify'
, runState
, StateC(..)
) where

import Control.Effect.Handler
import Control.Effect.Sum
import Control.Effect.Internal
import Data.Coerce

data State s m k
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
get = send (Get gen)

-- | Project a function out of the current state value.
--
--   prop> snd (run (runState a (gets (applyFun f)))) == applyFun f a
gets :: (Member (State s) sig, Carrier sig m, Functor m) => (s -> a) -> m a
gets f = fmap f get

-- | Replace the state value with a new value.
--
--   prop> fst (run (runState a (put b))) == b
--   prop> snd (run (runState a (get <* put b))) == a
--   prop> snd (run (runState a (put b *> get))) == b
put :: (Member (State s) sig, Carrier sig m) => s -> m ()
put s = send (Put s (gen ()))

-- | Replace the state value with the result of applying a function to the current state value.
--
--   prop> fst (run (runState a (modify (+1)))) == (1 + a :: Integer)
modify :: (Member (State s) sig, Carrier sig m, Monad m) => (s -> s) -> m ()
modify f = get >>= put . f

-- | Like 'modify', but strict in the new state.
--
--   prop> fst (run (runState a (modify' (+1)))) == (1 + a :: Integer)
modify' :: (Member (State s) sig, Carrier sig m, Monad m) => (s -> s) -> m ()
modify' f = do
  a <- get
  put $! f a


-- | Run a 'State' effect starting from the passed value.
--
--   prop> run (runState a (pure b)) == (a, b)
runState :: (Carrier sig m, Effect sig) => s -> Eff (StateC s m) a -> m (s, a)
runState s m = runStateC (interpret m) s

newtype StateC s m a = StateC { runStateC :: s -> m (s, a) }

instance (Carrier sig m, Effect sig) => Carrier (State s :+: sig) (StateC s m) where
  gen a = StateC (\ s -> gen (s, a))
  alg = algS \/ algOther
    where algS (Get   k) = StateC (\ s -> runStateC (k s) s)
          algS (Put s k) = StateC (\ _ -> runStateC  k    s)
          algOther op = StateC (\ s -> alg (handle (s, ()) (uncurry (flip runStateC)) op))


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
