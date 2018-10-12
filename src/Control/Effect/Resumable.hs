{-# LANGUAGE ExistentialQuantification #-}
module Control.Effect.Resumable where

data Resumable exc k
  = forall a . Resumable (exc a) (a -> k)
