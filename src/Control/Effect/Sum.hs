{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DeriveTraversable, DerivingStrategies, FlexibleInstances, KindSignatures, MultiParamTypeClasses, TypeOperators #-}
module Control.Effect.Sum
( (:+:)(..)
, Member(..)
, send
) where

import Control.Effect.Carrier
import GHC.Generics (Generic1)

data (f :+: g) (m :: * -> *) k
  = L (f m k)
  | R (g m k)
  deriving stock (Eq, Foldable, Functor, Generic1, Ord, Show, Traversable)
  deriving anyclass (HFunctor, Effect)

infixr 4 :+:

class Member (sub :: (* -> *) -> (* -> *)) sup where
  inj :: sub m a -> sup m a
  prj :: sup m a -> Maybe (sub m a)

instance Member sub sub where
  inj = id
  prj = Just

instance {-# OVERLAPPABLE #-} Member sub (sub :+: sup) where
  inj = L . inj
  prj (L f) = Just f
  prj _     = Nothing

instance {-# OVERLAPPABLE #-} Member sub sup => Member sub (sub' :+: sup) where
  inj = R . inj
  prj (R g) = prj g
  prj _     = Nothing


-- | Construct a request for an effect to be interpreted by some handler later on.
send :: (Member effect sig, Carrier sig m) => effect m a -> m a
send = eff . inj
{-# INLINE send #-}
