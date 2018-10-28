{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Effect.Rand
( Rand(..)
, runRand
, evalRand
, execRand
, RandC(..)
) where

import Control.Effect.Carrier
import Control.Effect.Internal
import Control.Effect.Rand.Internal
import Control.Effect.Sum
import System.Random (Random(..), RandomGen(..))

runRand :: (Carrier sig m, Effect sig, RandomGen g) => g -> Eff (RandC g m) a -> m (g, a)
runRand g = flip runRandC g . interpret

evalRand :: (Carrier sig m, Effect sig, Functor m, RandomGen g) => g -> Eff (RandC g m) a -> m a
evalRand g = fmap snd . runRand g

execRand :: (Carrier sig m, Effect sig, Functor m, RandomGen g) => g -> Eff (RandC g m) a -> m g
execRand g = fmap fst . runRand g

newtype RandC g m a = RandC { runRandC :: g -> m (g, a) }

instance (Carrier sig m, Effect sig, RandomGen g) => Carrier (Rand :+: sig) (RandC g m) where
  ret a = RandC (\ g -> ret (g, a))
  eff op = RandC (\ g -> (alg g \/ eff . handleState g runRandC) op)
    where alg g (Uniform    k) = let (a, g') = random g in runRandC (k a) g'
          alg g (UniformR r k) = let (a, g') = randomR r g in runRandC (k a) g'
