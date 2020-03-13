{-# LANGUAGE DeriveFunctor #-}

{- | This effect provides source to an infinite source of 'Int' values, suitable for generating "fresh" values to uniquely identify data without needing to invoke random numbers or impure IO.

Predefined carriers:

* "Control.Carrier.Fresh.Strict".

-}
module Control.Effect.Fresh
( -- * Fresh effect
  Fresh(..)
, fresh
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra

-- | @since 0.1.0.0
newtype Fresh m k = Fresh (Int -> m k)
  deriving (Functor)


-- | Produce a fresh (i.e. unique) 'Int'.
--
-- @
-- m '>>' 'fresh' ≠ m '>>' 'fresh' '>>' 'fresh'
-- @
--
-- @since 0.1.0.0
fresh :: Has Fresh sig m => m Int
fresh = send (Fresh pure)
