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

data Eavesdrop eff m k
  = forall a . Eavesdrop (m a) (forall n x . eff n (n x) -> m ()) (a -> k)

deriving instance Functor (Eavesdrop eff m)

instance HFunctor (Eavesdrop eff) where
  hmap f (Eavesdrop m h k) = Eavesdrop (f m) (f . h) k

-- | Listen in on requests for some specific effect with a handler.
--
--   The intercepted effects are re-sent after the handler in the surrounding context; thus, nested 'eavesdrop's listening for the same effect will apply innermost-first, and the effect’s own handler will still get the chance to service the request. This necessarily also means that the 'Eavesdrop' effect must be run inside the handler for the target effect.
eavesdrop :: (Member (Eavesdrop eff) sig, Carrier sig m)
          => m a
          -> (forall n x . eff n (n x) -> m ())
          -> m a
eavesdrop m f = send (Eavesdrop m f ret)


runEavesdrop :: (HFunctor eff, Carrier sig m, Monad m, Member eff sig) => Eff (EavesdropC eff m) a -> m a
runEavesdrop = flip runEavesdropC Nothing . interpret

newtype EavesdropC eff m a = EavesdropC { runEavesdropC :: Maybe (Tap eff m) -> m a }

newtype Tap eff m = Tap { runTap :: forall n x . eff n (n x) -> m () }

instance Applicative m => Semigroup (Tap eff m) where
  Tap t1 <> Tap t2 = Tap (\ eff -> t1 eff *> t2 eff)

instance (Carrier sig m, HFunctor eff, Member eff sig, Monad m) => Carrier (Eavesdrop eff :+: sig) (EavesdropC eff m) where
  ret a = EavesdropC (const (ret a))
  eff op = EavesdropC (\ tap -> (alg tap \/ algOther tap) op)
    where alg tap (Eavesdrop m h k) = runEavesdropC m (Just (Tap (flip runEavesdropC tap . h)) <> tap) >>= flip runEavesdropC tap . k
          algOther tap op
            | Just tap <- tap
            , Just eff <- prj op = runTap tap eff *> send (handleReader (Just tap) runEavesdropC eff)
            | otherwise          = eff (handleReader tap runEavesdropC op)
