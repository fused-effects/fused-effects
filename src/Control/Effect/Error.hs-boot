{-# LANGUAGE ExistentialQuantification #-}
module Control.Effect.Error
( Error(..)
) where

import Control.Effect.Class

data Error exc m k
  = Throw exc
  | forall b . Catch (m b) (exc -> m b) (b -> m k)

instance HFunctor (Error exc)
