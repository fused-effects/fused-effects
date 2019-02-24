{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Control.Effect.Random
( Random(..)
, runRandom
, evalRandom
, execRandom
, evalRandomIO
, RandomC(..)
, MonadRandom(..)
, MonadInterleave(..)
) where

import Control.Effect.Carrier
import Control.Effect.Internal
import Control.Effect.Random.Internal
import Control.Effect.State
import Control.Effect.Sum
import Control.Monad.Fail
import Control.Monad.Random.Class (MonadInterleave(..), MonadRandom(..))
import Control.Monad.IO.Class (MonadIO(..))
import qualified System.Random as R (Random(..), RandomGen(..), StdGen, newStdGen)

-- | Run a random computation starting from a given generator.
--
--   prop> run (runRandom (PureGen a) (pure b)) == (PureGen a, b)
runRandom :: (Carrier sig m, Effect sig, Monad m, R.RandomGen g) => g -> Eff (RandomC g m) a -> m (g, a)
runRandom g = flip runStateC g . runRandomC . interpret

-- | Run a random computation starting from a given generator and discarding the final generator.
--
--   prop> run (evalRandom (PureGen a) (pure b)) == b
evalRandom :: (Carrier sig m, Effect sig, Monad m, R.RandomGen g) => g -> Eff (RandomC g m) a -> m a
evalRandom g = fmap snd . runRandom g

-- | Run a random computation starting from a given generator and discarding the final result.
--
--   prop> run (execRandom (PureGen a) (pure b)) == PureGen a
execRandom :: (Carrier sig m, Effect sig, Monad m, R.RandomGen g) => g -> Eff (RandomC g m) a -> m g
execRandom g = fmap fst . runRandom g

-- | Run a random computation in 'IO', splitting the global standard generator to get a new one for the computation.
evalRandomIO :: (Carrier sig m, Effect sig, MonadIO m) => Eff (RandomC R.StdGen m) a -> m a
evalRandomIO m = liftIO R.newStdGen >>= flip evalRandom m

newtype RandomC g m a = RandomC { runRandomC :: StateC g m a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance (Carrier sig m, Effect sig, R.RandomGen g, Monad m) => Carrier (Random :+: sig) (RandomC g m) where
  ret = pure
  eff = RandomC . handleSum (eff . R . handleCoercible) (\case
    Random    k -> do
      (a, g') <- gets R.random
      put (g' :: g)
      runRandomC (k a)
    RandomR r k -> do
      (a, g') <- gets (R.randomR r)
      put (g' :: g)
      runRandomC (k a)
    Interleave m k -> do
      (g1, g2) <- gets R.split
      put (g1 :: g)
      a <- runRandomC m
      put g2
      runRandomC (k a))


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import System.Random
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
-- >>> import Control.Effect.NonDet
-- >>> newtype PureGen = PureGen Int deriving (Eq, Show)
-- >>> instance RandomGen PureGen where next (PureGen i) = (i, PureGen i) ; split g = (g, g)
