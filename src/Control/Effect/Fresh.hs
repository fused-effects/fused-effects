{-# LANGUAGE DeriveFunctor, FlexibleContexts, StandaloneDeriving #-}

{- | This effect provides source to an infinite source of 'Int' values, suitable for generating "fresh" values to uniquely identify data without needing to invoke random numbers or impure IO.

Predefined carriers:

* "Control.Carrier.Fresh.Strict".

-}
module Control.Effect.Fresh
( -- * Fresh effect
  Fresh(..)
, fresh
  -- * Re-exports
, Carrier
, Has
, run
) where

import Control.Carrier

-- | @since 0.1.0.0
data Fresh m k
  = Fresh (Int -> m k)

deriving instance Functor m => Functor (Fresh m)

instance HFunctor Fresh where
  hmap f (Fresh   k) = Fresh       (f . k)

instance Effect Fresh where
  handle state handler (Fresh   k) = Fresh (handler . (<$ state) . k)


-- | Produce a fresh (i.e. unique) 'Int'.
--
-- @
-- m '>>' 'fresh' ≠ m '>>' 'fresh' '>>' 'fresh'
-- @
--
-- @since 0.1.0.0
fresh :: Has Fresh sig m => m Int
fresh = send (Fresh pure)
