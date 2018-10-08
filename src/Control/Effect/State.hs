{-# LANGUAGE DeriveFunctor, FlexibleContexts, MultiParamTypeClasses, PolyKinds, TypeOperators #-}
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

get :: Subset (State s) sig => Eff sig s
get = send (Get pure)

put :: Subset (State s) sig => s -> Eff sig ()
put s = send (Put s (pure ()))

runState :: Effect sig => s -> Eff (State s :+: sig) a -> Eff sig (s, a)
runState s m = runStateH (relay alg m) s
  where alg (Get k)   = StateH (\ s -> runStateH (k s) s)
        alg (Put s k) = StateH (\ _ -> runStateH  k    s)


newtype StateH s m a = StateH { runStateH :: s -> m (s, a) }
  deriving (Functor)

instance Monad m => Applicative (StateH s m) where
  pure a = StateH (\ s -> pure (s, a))

  StateH f <*> StateH a = StateH $ \ s -> do
    (s',  f') <- f s
    (s'', a') <- a s'
    let fa = f' a'
    fa `seq` pure (s'', fa)

instance Monad m => Monad (StateH s m) where
  return = pure

  StateH a >>= f = StateH $ \ s -> do
    (s', a') <- a s
    let fa = f a'
    fa `seq` runStateH fa s'

instance Carrier (StateH s) ((,) s) where
  joinl mf = StateH (\ s -> mf >>= \ f -> runStateH f s)

  suspend = StateH (\ s -> pure (s, (s, ())))

  resume (s, m) = runStateH m s

  wrap = StateH . const
