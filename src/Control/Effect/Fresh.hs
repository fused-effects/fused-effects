{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, StandaloneDeriving #-}

{- | This effect provides source to an infinite source of 'Int' values, suitable for generating "fresh" values to uniquely identify data without needing to invoke random numbers or impure IO.

Predefined carriers:

* "Control.Carrier.Fresh.Strict".

-}
module Control.Effect.Fresh
( -- * Fresh effect
  Fresh(..)
, fresh
, resetFresh
  -- * Re-exports
, Carrier
, Has
) where

import Control.Carrier

-- | @since 0.1.0.0
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
--
-- @since 0.1.0.0
fresh :: Has Fresh sig m => m Int
fresh = send (Fresh pure)

-- | Reset the fresh counter after running a computation.
--
--   prop> run (runFresh (resetFresh (replicateM m fresh) *> replicateM n fresh)) === run (runFresh (replicateM n fresh))
--
-- @since 0.1.0.0
resetFresh :: Has Fresh sig m => m a -> m a
resetFresh m = send (Reset m pure)


-- $setup
-- >>> import Test.QuickCheck
-- >>> import Control.Carrier.Fresh.Strict
-- >>> import Control.Monad (replicateM)
-- >>> import Data.List (nub)
