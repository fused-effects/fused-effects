{-# LANGUAGE DeriveFunctor, DeriveGeneric, ExistentialQuantification, RankNTypes, TypeFamilies #-}
module Control.Effect.Lift.Internal
( Lift(..)
, Unlift(..)
) where

import Control.Effect.Class
import Data.Functor.Identity
import GHC.Generics (Generic1)

-- | @since 0.1.0.0
newtype Lift sig m k = Lift { unLift :: sig (m k) }
  deriving (Functor, Generic1)

instance Functor m => Effect (Lift m)


data Unlift sig m k
  = forall a . Unlift ((forall a . m a -> sig a) -> m a) (a -> m k)

instance Functor m => Functor (Unlift sig m) where
  fmap f (Unlift with k) = Unlift with (fmap f . k)

instance Effect (Unlift sig) where
  type CanHandle (Unlift sig) ctx = ctx ~ Identity
  handle ctx dst (Unlift with k) = Unlift (\ run -> dst (with (run . fmap runIdentity . dst . (<$ ctx)) <$ ctx)) (dst . fmap k)
