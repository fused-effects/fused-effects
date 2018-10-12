{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, PolyKinds, StandaloneDeriving #-}
module Control.Effect.Resumable
( Resumable(..)
, throwResumable
, SomeExc(..)
) where

import Control.Effect.Handler
import Control.Effect.Sum

data Resumable exc m k
  = forall a . Resumable (exc a) (a -> k)

deriving instance Functor (Resumable exc m)

instance HFunctor (Resumable exc) where
  hfmap _ (Resumable exc k) = Resumable exc k

instance Effect (Resumable exc) where
  handle state handler (Resumable exc k) = Resumable exc (handler . (<$ state) . k)

throwResumable :: (Member (Resumable exc) sig, Carrier sig m) => exc a -> m a
throwResumable exc = send (Resumable exc gen)


data SomeExc exc
  = forall a . SomeExc (exc a)
