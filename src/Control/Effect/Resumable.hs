{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Resumable
( Resumable(..)
, throwResumable
, SomeExc(..)
, runResumable
, ResumableH(..)
) where

import Control.Effect.Handler
import Control.Effect.Internal
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


data SomeExc (exc :: * -> *)
  = forall a . SomeExc (exc a)


runResumable :: Effectful sig m => Eff (ResumableH exc m) a -> m (Either (SomeExc exc) a)
runResumable = runResumableH . interpret

newtype ResumableH exc m a = ResumableH { runResumableH :: m (Either (SomeExc exc) a) }

instance Effectful sig m => Carrier (Resumable exc :+: sig) (ResumableH exc m) where
  gen a = ResumableH (gen (Right a))
  alg = algE \/ (ResumableH . alg . handle (Right ()) (either (gen . Left) runResumableH))
    where algE (Resumable exc _) = ResumableH (gen (Left (SomeExc exc)))
