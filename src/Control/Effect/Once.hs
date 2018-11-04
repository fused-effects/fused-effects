{-# LANGUAGE ExistentialQuantification #-}
module Control.Effect.Once where

data Once m k
  = forall a . Once (m a) (a -> k)
