{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Fresh
( -- * Fresh effect
  Fresh(..)
, fresh
, resetFresh
  -- * Fresh carrier
, runFresh
, FreshC(..)
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Control.Effect.State
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

data Fresh m k
  = Fresh (Int -> m k)
  | forall b . Reset (m b) (b -> m k)

deriving instance Functor m => Functor (Fresh m)

instance HFunctor Fresh where
  hmap f (Fresh   k) = Fresh       (f . k)
  hmap f (Reset m k) = Reset (f m) (f . k)

instance Effect Fresh where
  handle state handler (Fresh   k) = Fresh (handler . (<$ state) . k)
  handle state handler (Reset m k) = Reset (handler (m <$ state)) (handler . fmap k)

-- | Produce a fresh (i.e. unique) 'Int'.
--
--   prop> run (runFresh (replicateM n fresh)) === nub (run (runFresh (replicateM n fresh)))
fresh :: (Member Fresh sig, Carrier sig m) => m Int
fresh = send (Fresh pure)

-- | Reset the fresh counter after running a computation.
--
--   prop> run (runFresh (resetFresh (replicateM m fresh) *> replicateM n fresh)) === run (runFresh (replicateM n fresh))
resetFresh :: (Member Fresh sig, Carrier sig m) => m a -> m a
resetFresh m = send (Reset m pure)


-- | Run a 'Fresh' effect counting up from 0.
--
--   prop> run (runFresh (replicateM n fresh)) === [0..pred n]
--   prop> run (runFresh (replicateM n fresh *> pure b)) === b
runFresh :: Functor m => FreshC m a -> m a
runFresh = evalState 0 . runFreshC

newtype FreshC m a = FreshC { runFreshC :: StateC Int m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Carrier sig m, Effect sig) => Carrier (Fresh :+: sig) (FreshC m) where
  eff (L (Fresh   k)) = FreshC $ do
    i <- get
    put (succ i)
    runFreshC (k i)
  eff (L (Reset m k)) = FreshC $ do
    i <- get
    a <- runFreshC m
    put (i :: Int)
    runFreshC (k a)
  eff (R other)       = FreshC (eff (R (handleCoercible other)))
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
-- >>> import Control.Monad (replicateM)
-- >>> import Data.List (nub)
