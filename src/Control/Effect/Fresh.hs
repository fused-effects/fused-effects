{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Fresh
( Fresh(..)
, fresh
, resetFresh
, runFresh
, FreshC(..)
) where

import Control.Effect.Carrier
import Control.Effect.Internal
import Control.Effect.Sum

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
fresh = send (Fresh gen)

-- | Reset the fresh counter after running a computation.
--
--   prop> run (runFresh (resetFresh (replicateM m fresh) *> replicateM n fresh)) == run (runFresh (replicateM n fresh))
resetFresh :: (Member Fresh sig, Carrier sig m) => m a -> m a
resetFresh m = send (Reset m gen)


-- | Run a 'Fresh' effect counting up from 0.
--
--   prop> run (runFresh (replicateM n fresh)) == [0..pred n]
--   prop> run (runFresh (replicateM n fresh *> pure b)) == b
runFresh :: (Carrier sig m, Effect sig, Monad m) => Eff (FreshC m) a -> m a
runFresh = fmap snd . flip runFreshC 0 . interpret

newtype FreshC m a = FreshC { runFreshC :: Int -> m (Int, a) }

instance (Carrier sig m, Effect sig, Monad m) => Carrier (Fresh :+: sig) (FreshC m) where
  gen a = FreshC (\ i -> gen (i, a))
  alg = algF \/ algOther
    where algF (Fresh   k) = FreshC (\ i -> runFreshC (k i) (succ i))
          algF (Reset m k) = FreshC (\ i -> runFreshC m i >>= \ (_, a) -> runFreshC (k a) i)
          algOther op = FreshC (\ i -> alg (handle (i, ()) (uncurry (flip runFreshC)) op))


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
-- >>> import Control.Monad (replicateM)
-- >>> import Data.List (nub)
