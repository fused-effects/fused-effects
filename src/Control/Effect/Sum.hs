{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DeriveGeneric, DeriveTraversable, FlexibleContexts, FlexibleInstances, KindSignatures, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
-- | Operations on /sums/, combining effects into a /signature/.
module Control.Effect.Sum
( -- * Membership
  Member(..)
  -- * Sums
, (:+:)(..)
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
--
--   This is based on Wouter Swierstra’s design described in [Data types à la carte](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf). As described therein, overlapping instances are required in order to distinguish e.g. left-occurrence from right-recursion.
--
--   It should not generally be necessary for you to define new 'Member' instances, but these are not specifically prohibited if you wish to get creative.
class Member (sub :: (* -> *) -> (* -> *)) sup where
  -- | Inject a member of a signature into the signature.
  inj :: sub m a -> sup m a

instance {-# OVERLAPPABLE #-} Member sub sub where
  inj = id

instance (Member' elem sub sup, Elem sub sup ~ elem) => Member sub sup where
  inj = inj' @elem


type family Elem (sub :: (* -> *) -> (* -> *)) sup :: Bool where
  Elem t t         = 'True
  Elem t (l :+: r) = Elem t l || Elem t r
  Elem _ _         = 'False

type family (a :: Bool) || (b :: Bool) where
  'False || 'False = 'False
  _      || _      = 'True


class Member' (elem :: Bool) (sub :: (* -> *) -> (* -> *)) sup where
  inj' :: sub m a -> sup m a

-- | Reflexivity: @t@ is a member of itself.
instance Member' 'True t t where
  inj' = id

-- | Left-recursion: if @t@ is a member of @l1 ':+:' l2 ':+:' r@, then we can inject it into @(l1 ':+:' l2) ':+:' r@ by injection into a right-recursive signature, followed by left-association.
instance {-# OVERLAPPABLE #-}
         Member' 'True t (l1 :+: l2 :+: r)
      => Member' 'True t ((l1 :+: l2) :+: r) where
  inj' = reassoc . inj' @'True where
    reassoc (L l)     = L (L l)
    reassoc (R (L l)) = L (R l)
    reassoc (R (R r)) = R r

-- | Left-occurrence: if @t@ is at the head of a signature, we can inject it in O(1).
instance {-# OVERLAPPABLE #-}
         Member' 'True l (l :+: r) where
  inj' = L

-- | Right-recursion: if @t@ is a member of @r@, we can inject it into @r@ in O(n), followed by lifting that into @l ':+:' r@ in O(1).
instance {-# OVERLAPPABLE #-}
         Member' 'True l r
      => Member' 'True l (l' :+: r) where
  inj' = R . inj' @'True
