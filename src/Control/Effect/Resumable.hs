{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, PolyKinds, RankNTypes, StandaloneDeriving, TypeOperators #-}
module Control.Effect.Resumable where

import Control.Carrier.Identity
import Control.Effect

data Resumable exc m k
  = forall b . Resumable (exc b) (b -> k)

deriving instance Functor (Resumable exc m)

instance Effect (Resumable exc) where
  hfmap _ (Resumable exc k) = Resumable exc k

  handle _ (Resumable exc k) = Resumable exc k

throwResumable :: Subset (Resumable exc) sig => exc a -> Eff sig a
throwResumable exc = send (Resumable exc pure)


runResumable :: Effect sig => (forall resume . exc resume -> Eff sig resume) -> Eff (Resumable exc :+: sig) a -> Eff sig a
runResumable f = runIdentityH . relay alg
  where alg (Resumable exc k) = IdentityH (f exc >>= runIdentityH . k)
