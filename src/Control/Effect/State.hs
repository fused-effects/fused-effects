{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Control.Effect.State where

import Control.Effect
import Control.Monad.Codensity

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
  deriving (Functor)

instance Monad m => Applicative (StateH s m) where
  pure a = StateH (\ s -> pure (s, a))

  StateH f <*> StateH a = StateH $ \ s -> do
    (s',  f') <- f s
    (s'', a') <- a s'
    let fa = f' a'
    fa `seq` pure (s'', fa)

instance Carrier ((,) s) (StateH s) where
  joinl mf = StateH (\ s -> mf >>= \ f -> runStateH f s)

  suspend f = StateH (\ s -> runStateH (f (s, ())) s)

  resume (s, m) = runStateH m s

  wrap = StateH . const

instance TermMonad m sig => TermAlgebra (StateH s m) (State s :+: sig) where
  var = pure
  con = alg \/ interpretRest
    where alg (Get   k) = StateH (\ s -> runStateH (k s) s)
          alg (Put s k) = StateH (\ _ -> runStateH  k    s)
