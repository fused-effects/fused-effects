{-# LANGUAGE DeriveFunctor, FlexibleContexts, PolyKinds #-}
module Control.Effect.Trace
( Trace(..)
, trace
) where

import Control.Effect.Handler
import Control.Effect.Sum

data Trace m k = Trace String k
  deriving (Functor)

instance HFunctor Trace where
  hfmap _ (Trace s k) = Trace s k

trace :: (Subset Trace sig, Effectful sig m) => String -> m ()
trace message = send (Trace message (pure ()))
