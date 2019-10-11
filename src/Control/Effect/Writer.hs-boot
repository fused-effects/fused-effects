{-# LANGUAGE ExistentialQuantification #-}
module Control.Effect.Writer
( Writer(..)
) where

import Control.Effect.Class

data Writer w m k
  = Tell w (m k)
  | forall a . Listen (m a) (w -> a -> m k)
  | forall a . Censor (w -> w) (m a) (a -> m k)

instance HFunctor (Writer w)
