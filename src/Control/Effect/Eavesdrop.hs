{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Eavesdrop
( Eavesdrop(..)
, eavesdrop
, runEavesdrop
, EavesdropC(..)
, Tap(..)
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
            | _:_ <- taps
            , Just eff <- prj op = traverse_ (flip runTap eff) taps *> send (handleReader taps runEavesdropC eff)
            | otherwise          = eff (handleReader taps runEavesdropC op)
