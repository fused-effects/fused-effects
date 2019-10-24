{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Control.Effect.State
( State(..)
) where

import Control.Effect.Class

data State s m k
  = Get (s -> m k)
  | Put s (m k)

instance Functor f => Effect f (State s)
