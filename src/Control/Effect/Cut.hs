{-# LANGUAGE DeriveFunctor, ExistentialQuantification, StandaloneDeriving #-}
module Control.Effect.Cut
( Cut(..)
) where

data Cut m k
  = Cutfail
  | forall a . Call (m a) (a -> k)

deriving instance Functor (Cut m)
