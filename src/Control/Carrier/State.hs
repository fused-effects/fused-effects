{-# LANGUAGE DeriveFunctor, MultiParamTypeClasses #-}
module Control.Carrier.State where

import Control.Carrier

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
