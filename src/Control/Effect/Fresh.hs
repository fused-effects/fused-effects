{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, PolyKinds, StandaloneDeriving #-}
module Control.Effect.Fresh
( Fresh(..)
, fresh
, resetFresh
) where

import Control.Effect.Handler
import Control.Effect.Sum

data Fresh m k
  = Fresh (Int -> k)
  | forall b . Reset Int (m b) (b -> k)

deriving instance Functor (Fresh m)

instance HFunctor Fresh where
  hfmap _ (Fresh     k) = Fresh k
  hfmap f (Reset i m k) = Reset i (f m) k

instance Effect Fresh where
  handle state handler (Fresh     k) = Fresh (handler . (<$ state) . k)
  handle state handler (Reset i m k) = Reset i (handler (m <$ state)) (handler . fmap k)

fresh :: (Subset Fresh sig, Effectful sig m) => m Int
fresh = send (Fresh pure)

resetFresh :: (Subset Fresh sig, Effectful sig m) => Int -> m a -> m a
resetFresh i m = send (Reset i m pure)
