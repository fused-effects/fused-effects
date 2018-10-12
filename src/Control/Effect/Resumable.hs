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
import Data.Functor.Classes

data Resumable exc m k
  = forall a . Resumable (exc a) (a -> k)

deriving instance Functor (Resumable exc m)

instance HFunctor (Resumable exc) where
  hfmap _ (Resumable exc k) = Resumable exc k

instance Effect (Resumable exc) where
  handle state handler (Resumable exc k) = Resumable exc (handler . (<$ state) . k)

-- | Throw an exception which can be resumed with a value of its result type.
throwResumable :: (Member (Resumable exc) sig, Carrier sig m) => exc a -> m a
throwResumable exc = send (Resumable exc gen)


data SomeExc (exc :: * -> *)
  = forall a . SomeExc (exc a)

instance Eq1 exc => Eq (SomeExc exc) where
  SomeExc exc1 == SomeExc exc2 = liftEq (const (const True)) exc1 exc2

instance (Show1 exc) => Show (SomeExc exc) where
  showsPrec num (SomeExc exc) = liftShowsPrec (const (const id)) (const id) num exc


runResumable :: Effectful sig m => Eff (ResumableH exc m) a -> m (Either (SomeExc exc) a)
runResumable = runResumableH . interpret

newtype ResumableH exc m a = ResumableH { runResumableH :: m (Either (SomeExc exc) a) }

instance Effectful sig m => Carrier (Resumable exc :+: sig) (ResumableH exc m) where
  gen a = ResumableH (gen (Right a))
  alg = algE \/ (ResumableH . alg . handle (Right ()) (either (gen . Left) runResumableH))
    where algE (Resumable exc _) = ResumableH (gen (Left (SomeExc exc)))
