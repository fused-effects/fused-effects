{-# LANGUAGE DeriveFunctor, FlexibleContexts, PolyKinds, TypeOperators #-}
module Control.Effect.State where

import Control.Carrier.State
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

get :: Subset (State s) sig => Eff sig s
get = send (Get pure)

put :: Subset (State s) sig => s -> Eff sig ()
put s = send (Put s (pure ()))


runState :: Effect sig => s -> Eff (State s :+: sig) a -> Eff sig (s, a)
runState s m = runStateH (interpret alg m) s
  where alg (Get k)   = StateH (\ s -> runStateH (k s) s)
        alg (Put s k) = StateH (\ _ -> runStateH  k    s)
