{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Control.Effect.State
( State(..)
, get
, put
, modify
, runState
, StateH(..)
) where

import Control.Effect.Handler
import Control.Effect.Sum
import Control.Effect.Internal

data State s m k
  = Get (s -> k)
  | Put s k
  deriving (Functor)

instance HFunctor (State s) where
  hfmap _ (Get k)   = Get   k
  hfmap _ (Put s k) = Put s k

instance Effect (State s) where
  handle state handler (Get k)   = Get   (handler . (<$ state) . k)
  handle state handler (Put s k) = Put s (handler . (<$ state) $ k)

-- | Get the current state value.
--
--   prop> snd (run (runState a get)) == a
get :: (Member (State s) sig, Carrier sig m) => m s
get = send (Get gen)

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
modify f = do
  a <- get
  put (f a)


runState :: (Carrier sig m, Effect sig) => s -> Eff (StateH s m) a -> m (s, a)
runState s m = runStateH (interpret m) s

newtype StateH s m a = StateH { runStateH :: s -> m (s, a) }

instance (Carrier sig m, Effect sig) => Carrier (State s :+: sig) (StateH s m) where
  gen a = StateH (\ s -> gen (s, a))
  alg = algS \/ algOther
    where algS (Get   k) = StateH (\ s -> runStateH (k s) s)
          algS (Put s k) = StateH (\ _ -> runStateH  k    s)
          algOther op = StateH (\ s -> alg (handle (s, ()) (uncurry (flip runStateH)) op))


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
