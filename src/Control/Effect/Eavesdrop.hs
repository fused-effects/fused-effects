{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Eavesdrop
( Eavesdrop(..)
, eavesdrop
, runEavesdrop
, EavesdropC(..)
, Tap(..)
, Interpose(..)
, interpose
, runInterpose
, InterposeC(..)
, Listener(..)
) where

import Control.Effect.Carrier
import Control.Effect.Internal
import Control.Effect.Sum
import Data.Foldable (traverse_)

data Eavesdrop eff m k
  = forall a . Eavesdrop (m a) (forall n x . eff n (n x) -> m ()) (a -> k)

deriving instance Functor (Eavesdrop eff m)

instance HFunctor (Eavesdrop eff) where
  hmap f (Eavesdrop m h k) = Eavesdrop (f m) (f . h) k

eavesdrop :: (Member (Eavesdrop eff) sig, Carrier sig m)
          => m a
          -> (forall n x . eff n (n x) -> m ())
          -> m a
eavesdrop m f = send (Eavesdrop m f ret)


runEavesdrop :: (HFunctor eff, Carrier sig m, Monad m, Member eff sig) => Eff (EavesdropC eff m) a -> m a
runEavesdrop = flip runEavesdropC [] . interpret

newtype EavesdropC eff m a = EavesdropC { runEavesdropC :: [Tap eff m] -> m a }

newtype Tap eff m = Tap { runTap :: forall n x . eff n (n x) -> m () }

instance (Carrier sig m, HFunctor eff, Member eff sig, Monad m) => Carrier (Eavesdrop eff :+: sig) (EavesdropC eff m) where
  ret a = EavesdropC (const (ret a))
  eff op = EavesdropC (\ taps -> (alg taps \/ algOther taps) op)
    where alg taps (Eavesdrop m h k) = runEavesdropC m (Tap (flip runEavesdropC taps . h) : taps) >>= flip runEavesdropC taps . k
          algOther taps op
            | not (null taps)
            , Just eff <- prj op = traverse_ (flip runTap eff) taps *> send (handleReader taps runEavesdropC eff)
            | otherwise          = eff (handleReader taps runEavesdropC op)


data Interpose eff m k
  = forall a . Interpose (m a) (forall n x . eff n (n x) -> m x) (a -> k)

deriving instance Functor (Interpose eff m)

instance HFunctor (Interpose eff) where
  hmap f (Interpose m h k) = Interpose (f m) (f . h) k

interpose :: (Member (Interpose eff) sig, Carrier sig m)
          => m a
          -> (forall n x . eff n (n x) -> m x)
          -> m a
interpose m f = send (Interpose m f ret)


runInterpose :: (Member eff sig, Carrier sig m, Monad m) => Eff (InterposeC eff m) a -> m a
runInterpose = flip runInterposeC Nothing . interpret

newtype InterposeC eff m a = InterposeC { runInterposeC :: Maybe (Listener eff m) -> m a }

newtype Listener eff m = Listener { runListener :: forall n x . eff n (n x) -> m x }

instance (Carrier sig m, Member eff sig, Monad m) => Carrier (Interpose eff :+: sig) (InterposeC eff m) where
  ret a = InterposeC (const (ret a))
  eff op = InterposeC (\ listener -> (alg listener \/ algOther listener) op)
    where alg listener (Interpose m h k) = runInterposeC m (Just (Listener (flip runInterposeC listener . h))) >>= flip runInterposeC listener . k
          algOther listener op
            | Just listener <- listener
            , Just eff <- prj op = runListener listener eff
            | otherwise          = eff (handleReader listener runInterposeC op)
