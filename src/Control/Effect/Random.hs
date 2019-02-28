{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
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

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Control.Effect.State
import Control.Effect.Sum
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.Random.Class (MonadInterleave(..), MonadRandom(..))
import Control.Monad.IO.Class (MonadIO(..))
import qualified System.Random as R (Random(..), RandomGen(..), StdGen, newStdGen)

data Random m k
  = forall a . R.Random a => Random (a -> k)
  | forall a . R.Random a => RandomR (a, a) (a -> k)
  | forall a . Interleave (m a) (a -> k)

deriving instance Functor (Random m)

instance HFunctor Random where
  hmap _ (Random k) = Random k
  hmap _ (RandomR r k) = RandomR r k
  hmap f (Interleave m k) = Interleave (f m) k
  {-# INLINE hmap #-}

instance Effect Random where
  handle state handler (Random    k) = Random    (handler . (<$ state) . k)
  handle state handler (RandomR r k) = RandomR r (handler . (<$ state) . k)
  handle state handler (Interleave m k) = Interleave (handler (m <$ state)) (handler . fmap k)


-- | Run a random computation starting from a given generator.
--
--   prop> run (runRandom (PureGen a) (pure b)) == (PureGen a, b)
runRandom :: g -> RandomC g m a -> m (g, a)
runRandom g = runState g . runRandomC

-- | Run a random computation starting from a given generator and discarding the final generator.
--
--   prop> run (evalRandom (PureGen a) (pure b)) == b
evalRandom :: Functor m => g -> RandomC g m a -> m a
evalRandom g = fmap snd . runRandom g

-- | Run a random computation starting from a given generator and discarding the final result.
--
--   prop> run (execRandom (PureGen a) (pure b)) == PureGen a
execRandom :: Functor m => g -> RandomC g m a -> m g
execRandom g = fmap fst . runRandom g

-- | Run a random computation in 'IO', splitting the global standard generator to get a new one for the computation.
evalRandomIO :: MonadIO m => RandomC R.StdGen m a -> m a
evalRandomIO m = liftIO R.newStdGen >>= flip evalRandom m

newtype RandomC g m a = RandomC { runRandomC :: StateC g m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadIO, MonadPlus)

instance (Carrier sig m, Effect sig, R.RandomGen g) => MonadRandom (RandomC g m) where
  getRandom = send (Random pure)
  {-# INLINE getRandom #-}
  getRandomR r = send (RandomR r pure)
  {-# INLINE getRandomR #-}
  getRandomRs interval = (:) <$> getRandomR interval <*> getRandomRs interval
  {-# INLINE getRandomRs #-}
  getRandoms = (:) <$> getRandom <*> getRandoms
  {-# INLINE getRandoms #-}

instance (Carrier sig m, Effect sig, R.RandomGen g) => MonadInterleave (RandomC g m) where
  interleave m = send (Interleave m pure)
  {-# INLINE interleave #-}

instance (Carrier sig m, Effect sig, R.RandomGen g) => Carrier (Random :+: sig) (RandomC g m) where
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
