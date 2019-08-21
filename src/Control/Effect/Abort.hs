{-# LANGUAGE DeriveFunctor, FlexibleContexts, KindSignatures #-}
module Control.Effect.Abort
( -- * Abort effect
  Abort(..)
, abort
  -- * Re-exports
, Carrier
, Member
, run
) where

import Control.Effect.Carrier

data Abort (m :: * -> *) k = Abort
  deriving (Functor)

-- | Abort the computation.
abort :: (Carrier sig m, Member Abort sig) => m a
abort = send Abort
