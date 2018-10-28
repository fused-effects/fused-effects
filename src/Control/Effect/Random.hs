{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Effect.Random
( Random(..)
, runRandom
, evalRandom
, execRandom
, RandomC(..)
, MonadRandom(..)
) where

import Control.Effect.Carrier
import Control.Effect.Internal
import Control.Effect.Random.Internal
import Control.Effect.Sum
import Control.Monad.Random.Class (MonadRandom(..))
import qualified System.Random as R (Random(..), RandomGen(..))

runRandom :: (Carrier sig m, Effect sig, R.RandomGen g) => g -> Eff (RandomC g m) a -> m (g, a)
runRandom g = flip runRandomC g . interpret

evalRandom :: (Carrier sig m, Effect sig, Functor m, R.RandomGen g) => g -> Eff (RandomC g m) a -> m a
evalRandom g = fmap snd . runRandom g

execRandom :: (Carrier sig m, Effect sig, Functor m, R.RandomGen g) => g -> Eff (RandomC g m) a -> m g
execRandom g = fmap fst . runRandom g

newtype RandomC g m a = RandomC { runRandomC :: g -> m (g, a) }

instance (Carrier sig m, Effect sig, R.RandomGen g) => Carrier (Random :+: sig) (RandomC g m) where
  ret a = RandomC (\ g -> ret (g, a))
  eff op = RandomC (\ g -> (alg g \/ eff . handleState g runRandomC) op)
    where alg g (Uniform    k) = let (a, g') = R.random g in runRandomC (k a) g'
          alg g (UniformR r k) = let (a, g') = R.randomR r g in runRandomC (k a) g'
