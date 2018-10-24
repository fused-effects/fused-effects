{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, RankNTypes, StandaloneDeriving #-}
module Control.Effect.Intercept
( Intercept(..)
, intercept
) where

import Control.Effect.Carrier
import Control.Effect.Sum

data Intercept eff m k
  = forall a . Intercept (m a) (forall n x . eff n (n x) -> m a) (a -> k)

deriving instance Functor (Intercept eff m)

instance HFunctor (Intercept eff) where
  hmap f (Intercept m h k) = Intercept (f m) (f . h) k

instance Effect (Intercept eff) where
  handle state handler (Intercept m h k) = Intercept (handler (m <$ state)) (handler . (<$ state) . h) (handler . fmap k)

intercept :: (Member (Intercept eff) sig, Carrier sig m)
          => m a
          -> (forall n x . eff n (n x) -> m a)
          -> m a
intercept m f = send (Intercept m f ret)
