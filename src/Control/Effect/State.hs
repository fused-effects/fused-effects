{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Control.Effect.State where

import Control.Effect

data State s m k
  = Get (s -> k)
  | Put s k
  deriving (Functor)

instance Effect (State s) where
  hfmap _ (Get k)   = Get   k
  hfmap _ (Put s k) = Put s k

  handle _ (Get k)   = Get   k
  handle _ (Put s k) = Put s k

get :: (Subset (State s) sig, TermMonad m sig) => m s
get = send (Get pure)

put :: (Subset (State s) sig, TermMonad m sig) => s -> m ()
put s = send (Put s (pure ()))


runState :: TermMonad m sig => s -> Codensity (StateH s m) a -> m (s, a)
runState s m = runStateH (runCodensity var m) s

newtype StateH s m a = StateH { runStateH :: s -> m (s, a) }

instance Carrier ((,) s) (StateH s) where
  joinl mf = StateH (\ s -> mf >>= \ f -> runStateH f s)

  suspend f = StateH (\ s -> runStateH (f (s, ())) s)

  resume (s, m) = runStateH m s

  wrap = StateH . const

  gen a = StateH (\ s -> pure (s, a))

instance TermMonad m sig => TermAlgebra (StateH s m) (State s :+: sig) where
  var = gen
  con = alg \/ interpretRest
    where alg (Get   k) = StateH (\ s -> runStateH (k s) s)
          alg (Put s k) = StateH (\ _ -> runStateH  k    s)
