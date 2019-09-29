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

-- | Left-recursion: if @t@ is a member of @l1 ':+:' l2 ':+:' r@, then we can inject it into @(l1 ':+:' l2) ':+:' r@ by injection into a right-recursive signature, followed by left-association.
instance {-# OVERLAPPABLE #-}
         Member t (l1 :+: l2 :+: r)
      => Member t ((l1 :+: l2) :+: r) where
  inj = reassoc . inj where
    reassoc (L l)     = L (L l)
    reassoc (R (L l)) = L (R l)
    reassoc (R (R r)) = R r

-- | Left occurrence: if @t@ is at the head of a signature, we can inject it in O(1).
instance {-# OVERLAPPABLE #-}
         Member l (l :+: r) where
  inj = L

-- | Right-recursion: if @t@ is a member of @r@, we can inject it into @r@ in O(n), followed by lifting that into @l :+: r@ in O(1).
instance {-# OVERLAPPABLE #-}
         Member l r
      => Member l (l' :+: r) where
  inj = R . inj
