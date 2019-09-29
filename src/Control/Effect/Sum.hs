{-# LANGUAGE DeriveGeneric, DeriveTraversable, FlexibleContexts, FlexibleInstances, KindSignatures, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Effect.Sum
( (:+:)(..)
, Member(..)
) where

import Control.Effect.Class
import GHC.Generics (Generic1)

-- | Higher-order sums are used to combine multiple effects into a signature, typically by chaining on the right.
data (f :+: g) (m :: * -> *) k
  = L (f m k)
  | R (g m k)
  deriving (Eq, Foldable, Functor, Generic1, Ord, Show, Traversable)

infixr 4 :+:

instance (HFunctor f, HFunctor g) => HFunctor (f :+: g)
instance (Effect f, Effect g)     => Effect   (f :+: g)


-- | The class of types present in a signature.
class Member (sub :: (* -> *) -> (* -> *)) sup where
  -- | Inject a member of a signature into the signature.
  inj :: sub m a -> sup m a

-- | Reflexivity: @t@ is a member of itself.
instance Member t t where
  inj = id

instance {-# OVERLAPPABLE #-}
         Member t (l1 :+: l2 :+: r)
      => Member t ((l1 :+: l2) :+: r) where
  inj = reassoc . inj where
    reassoc (L l)     = L (L l)
    reassoc (R (L l)) = L (R l)
    reassoc (R (R r)) = R r

instance {-# OVERLAPPABLE #-}
         Member l (l :+: r) where
  inj = L

instance {-# OVERLAPPABLE #-}
         Member l r
      => Member l (l' :+: r) where
  inj = R . inj
