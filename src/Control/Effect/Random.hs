{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Random
( -- * Random effect
  Random(..)
  -- * Random carrier
, runRandom
, evalRandom
, execRandom
, evalRandomIO
, RandomC(..)
  -- * Re-exports
, Carrier
, Member
, MonadRandom(..)
, MonadInterleave(..)
, run
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Control.Effect.State
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.Random.Class (MonadInterleave(..), MonadRandom(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class
import qualified System.Random as R (Random(..), RandomGen(..), StdGen, newStdGen)

data Random m k
  = forall a . R.Random a => Random (a -> m k)
  | forall a . R.Random a => RandomR (a, a) (a -> m k)
  | forall a . Interleave (m a) (a -> m k)

deriving instance Functor m => Functor (Random m)

instance HFunctor Random where
  hmap f (Random       k) = Random           (f . k)
  hmap f (RandomR r    k) = RandomR r        (f . k)
  hmap f (Interleave m k) = Interleave (f m) (f . k)
  {-# INLINE hmap #-}

instance Effect Random where
  handle state handler (Random       k) = Random                            (handler . (<$ state) . k)
  handle state handler (RandomR r    k) = RandomR r                         (handler . (<$ state) . k)
  handle state handler (Interleave m k) = Interleave (handler (m <$ state)) (handler . fmap k)


-- | Run a random computation starting from a given generator.
--
--   prop> run (runRandom (PureGen a) (pure b)) === (PureGen a, b)
runRandom :: g -> RandomC g m a -> m (g, a)
runRandom g = runState g . runRandomC

-- | Run a random computation starting from a given generator and discarding the final generator.
--
--   prop> run (evalRandom (PureGen a) (pure b)) === b
evalRandom :: Functor m => g -> RandomC g m a -> m a
evalRandom g = fmap snd . runRandom g

-- | Run a random computation starting from a given generator and discarding the final result.
--
--   prop> run (execRandom (PureGen a) (pure b)) === PureGen a
execRandom :: Functor m => g -> RandomC g m a -> m g
execRandom g = fmap fst . runRandom g

-- | Run a random computation in 'IO', splitting the global standard generator to get a new one for the computation.
evalRandomIO :: MonadIO m => RandomC R.StdGen m a -> m a
evalRandomIO m = liftIO R.newStdGen >>= flip evalRandom m

newtype RandomC g m a = RandomC { runRandomC :: StateC g m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Carrier sig m, Effect sig, R.RandomGen g) => MonadRandom (RandomC g m) where
  getRandom = RandomC $ do
    (a, g') <- gets R.random
    a <$ put (g' :: g)
  {-# INLINE getRandom #-}
  getRandomR r = RandomC $ do
    (a, g') <- gets (R.randomR r)
    a <$ put (g' :: g)
  {-# INLINE getRandomR #-}
  getRandomRs interval = (:) <$> getRandomR interval <*> getRandomRs interval
  {-# INLINE getRandomRs #-}
  getRandoms = (:) <$> getRandom <*> getRandoms
  {-# INLINE getRandoms #-}

instance (Carrier sig m, Effect sig, R.RandomGen g) => MonadInterleave (RandomC g m) where
  interleave m = RandomC $ do
    (g1, g2) <- gets R.split
    put (g1 :: g)
    a <- runRandomC m
    a <$ put g2
  {-# INLINE interleave #-}

instance (Carrier sig m, Effect sig, R.RandomGen g) => Carrier (Random :+: sig) (RandomC g m) where
  eff (L (Random       k)) = getRandom >>= k
  eff (L (RandomR r    k)) = getRandomR r >>= k
  eff (L (Interleave m k)) = interleave m >>= k
  eff (R other)            = RandomC (eff (R (handleCoercible other)))
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import System.Random
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
-- >>> import Control.Effect.NonDet
-- >>> newtype PureGen = PureGen Int deriving (Eq, Show)
-- >>> instance RandomGen PureGen where next (PureGen i) = (i, PureGen i) ; split g = (g, g)
