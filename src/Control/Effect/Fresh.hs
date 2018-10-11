{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Fresh
( Fresh(..)
, fresh
, resetFresh
, runFresh
, FreshH(..)
) where

import Control.Effect.Handler
import Control.Effect.Internal
import Control.Effect.Sum

data Fresh m k
  = Fresh (Int -> k)
  | forall b . Reset Int (m b) (b -> k)

deriving instance Functor (Fresh m)

instance HFunctor Fresh where
  hfmap _ (Fresh     k) = Fresh k
  hfmap f (Reset i m k) = Reset i (f m) k

instance Effect Fresh where
  handle state handler (Fresh     k) = Fresh (handler . (<$ state) . k)
  handle state handler (Reset i m k) = Reset i (handler (m <$ state)) (handler . fmap k)

fresh :: (Member Fresh sig, Effectful sig m) => m Int
fresh = send (Fresh pure)

resetFresh :: (Member Fresh sig, Effectful sig m) => Int -> m a -> m a
resetFresh i m = send (Reset i m pure)


runFresh :: Effectful sig m => Int -> Eff (FreshH m) a -> m a
runFresh i = fmap snd . flip runFreshH i . interpret

newtype FreshH m a = FreshH { runFreshH :: Int -> m (Int, a) }

instance Effectful sig m => Carrier (Fresh :+: sig) (FreshH m) where
  gen a = FreshH (\ i -> pure (i, a))
  alg = algF \/ algOther
    where algF (Fresh      k) = FreshH (\ i -> runFreshH (k i) (succ i))
          algF (Reset i' m k) = FreshH (\ i -> runFreshH m i' >>= \ (_, a) -> runFreshH (k a) i)
          algOther op = FreshH (\ i -> alg (handle (i, ()) (uncurry (flip runFreshH)) op))
