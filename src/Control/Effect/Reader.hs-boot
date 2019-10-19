{-# LANGUAGE ExistentialQuantification #-}
module Control.Effect.Reader
( Reader(..)
) where

import Control.Effect.Class

data Reader r m k
  = Ask (r -> m k)
  | forall b . Local (r -> r) (m b) (b -> m k)

instance HFunctor (Reader r)
