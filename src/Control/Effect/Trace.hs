{-# LANGUAGE DeriveFunctor, FlexibleContexts, PolyKinds #-}
module Control.Effect.Trace where

import Control.Effect.Handler
import Control.Effect.Sum

data Trace m k = Trace String k
  deriving (Functor)

trace :: (Subset Trace sig, Effectful sig m) => String -> m ()
trace message = send (Trace message (pure ()))
