{-# LANGUAGE DeriveFunctor, ExistentialQuantification, StandaloneDeriving #-}
module Control.Effect.Resumable where

data Resumable exc k
  = forall a . Resumable (exc a) (a -> k)

deriving instance Functor (Resumable exc)
