{-# LANGUAGE DeriveFunctor, ExistentialQuantification, PolyKinds, StandaloneDeriving #-}
module Control.Effect.Resumable
( Resumable(..)
) where

import Control.Effect.Handler

data Resumable exc m k
  = forall a . Resumable (exc a) (a -> k)

deriving instance Functor (Resumable exc m)

instance HFunctor (Resumable exc) where
  hfmap _ (Resumable exc k) = Resumable exc k
