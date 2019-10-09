module Control.Effect.State
( State(..)
) where

import Control.Effect.Class

data State s m k
  = Get (s -> m k)
  | Put s (m k)

instance Functor m => Functor (State s m)
instance HFunctor (State s)
instance Effect   (State s)
