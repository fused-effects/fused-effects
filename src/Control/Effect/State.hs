{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Control.Effect.State where

import Control.Effect

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

get :: (Subset (State s) sig, Effectful sig m) => m s
get = send (Get pure)

put :: (Subset (State s) sig, Effectful sig m) => s -> m ()
put s = send (Put s (pure ()))


runState :: Effectful sig m => s -> Eff (StateH s m) a -> m (s, a)
runState s m = runStateH (interpret m) s

newtype StateH s m a = StateH { runStateH :: s -> m (s, a) }

instance Effectful sig m => Carrier (State s :+: sig) (StateH s m) where
  gen a = StateH (\ s -> pure (s, a))
  con = alg \/ algOther
    where alg (Get   k) = StateH (\ s -> runStateH (k s) s)
          alg (Put s k) = StateH (\ _ -> runStateH  k    s)
          algOther op = StateH (\ s -> con (handle (s, ()) (uncurry (flip runStateH)) op))
