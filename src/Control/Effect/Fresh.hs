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
  | forall b . Reset (m b) (b -> k)

deriving instance Functor (Fresh m)

instance HFunctor Fresh where
  hfmap _ (Fresh   k) = Fresh k
  hfmap f (Reset m k) = Reset (f m) k

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
runFresh :: Effectful sig m => Eff (FreshH m) a -> m a
runFresh = fmap snd . flip runFreshH 0 . interpret

newtype FreshH m a = FreshH { runFreshH :: Int -> m (Int, a) }

instance Effectful sig m => Carrier (Fresh :+: sig) (FreshH m) where
  gen a = FreshH (\ i -> pure (i, a))
  alg = algF \/ algOther
    where algF (Fresh   k) = FreshH (\ i -> runFreshH (k i) (succ i))
          algF (Reset m k) = FreshH (\ i -> runFreshH m i >>= \ (_, a) -> runFreshH (k a) i)
          algOther op = FreshH (\ i -> alg (handle (i, ()) (uncurry (flip runFreshH)) op))


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
-- >>> import Control.Monad (replicateM)
-- >>> import Data.List (nub)
