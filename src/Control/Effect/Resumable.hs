{-# LANGUAGE DeriveFunctor, ExistentialQuantification, StandaloneDeriving #-}
module Control.Effect.Resumable
( Resumable(..)
) where

data Resumable exc m k
  = forall a . Resumable (exc a) (a -> k)

deriving instance Functor (Resumable exc m)
