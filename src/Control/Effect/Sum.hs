{-# LANGUAGE ConstraintKinds, DeriveGeneric, DeriveTraversable, FlexibleInstances, KindSignatures, MultiParamTypeClasses, TypeOperators #-}
module Control.Effect.Sum
( (:+:)(..)
, Member
, Inject(..)
, Project(..)
) where

import Control.Effect.Class
import GHC.Generics (Generic1)

data (f :+: g) (m :: * -> *) k
  = L (f m k)
  | R (g m k)
  deriving (Eq, Foldable, Functor, Generic1, Ord, Show, Traversable)

infixr 4 :+:

instance (HFunctor f, HFunctor g) => HFunctor (f :+: g)
instance (Effect f, Effect g)     => Effect   (f :+: g)


type Member sub sup = (Inject sub sup, Project sub sup)


class Inject (sub :: (* -> *) -> (* -> *)) sup where
  inj :: sub m a -> sup m a

instance Inject sub sub where
  inj = id

instance {-# OVERLAPPABLE #-} Inject sub (sub :+: sup) where
  inj = L . inj

instance {-# OVERLAPPABLE #-} Inject sub sup => Inject sub (sub' :+: sup) where
  inj = R . inj


class Project (sub :: (* -> *) -> (* -> *)) sup where
  prj :: sup m a -> Maybe (sub m a)

instance Project sub sub where
  prj = Just

instance {-# OVERLAPPABLE #-} Project sub (sub :+: sup) where
  prj (L f) = Just f
  prj _     = Nothing

instance {-# OVERLAPPABLE #-} Project sub sup => Project sub (sub' :+: sup) where
  prj (R g) = prj g
  prj _     = Nothing
