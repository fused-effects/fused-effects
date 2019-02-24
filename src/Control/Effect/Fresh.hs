{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Fresh
( Fresh(..)
, fresh
, resetFresh
, runFresh
, FreshC(..)
) where

import Control.Effect.Carrier
import Control.Effect.Internal
import Control.Effect.State
import Control.Effect.Sum
import Control.Monad.Fail

data Fresh m k
  = Fresh (Int -> k)
  | forall b . Reset (m b) (b -> k)

deriving instance Functor (Fresh m)

instance HFunctor Fresh where
  hmap _ (Fresh   k) = Fresh k
  hmap f (Reset m k) = Reset (f m) k

instance Effect Fresh where
  handle state handler (Fresh   k) = Fresh (handler . (<$ state) . k)
  handle state handler (Reset m k) = Reset (handler (m <$ state)) (handler . fmap k)

-- | Produce a fresh (i.e. unique) 'Int'.
--
--   prop> run (runFresh (replicateM n fresh)) == nub (run (runFresh (replicateM n fresh)))
fresh :: (Member Fresh sig, Carrier sig m) => m Int
fresh = send (Fresh ret)

-- | Reset the fresh counter after running a computation.
--
--   prop> run (runFresh (resetFresh (replicateM m fresh) *> replicateM n fresh)) == run (runFresh (replicateM n fresh))
resetFresh :: (Member Fresh sig, Carrier sig m) => m a -> m a
resetFresh m = send (Reset m ret)


-- | Run a 'Fresh' effect counting up from 0.
--
--   prop> run (runFresh (replicateM n fresh)) == [0..pred n]
--   prop> run (runFresh (replicateM n fresh *> pure b)) == b
runFresh :: (Carrier sig m, Effect sig, Monad m) => Eff (FreshC m) a -> m a
runFresh = fmap snd . flip runStateC 0 . runFreshC . interpret

newtype FreshC m a = FreshC { runFreshC :: StateC Int m a }
  deriving (Applicative, Functor, Monad, MonadFail)

instance (Carrier sig m, Effect sig, Monad m) => Carrier (Fresh :+: sig) (FreshC m) where
  ret = pure
  eff op = FreshC (StateC (\ i -> handleSum (eff . handleState i (runStateC . runFreshC)) (\case
    Fresh   k -> runStateC (runFreshC (k i)) (succ i)
    Reset m k -> runStateC (runFreshC m) i >>= flip runStateC i . runFreshC . k . snd) op))


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
-- >>> import Control.Monad (replicateM)
-- >>> import Data.List (nub)
