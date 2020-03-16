{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Operations on /sums/, combining effects into a /signature/.
--
-- @since 0.1.0.0
module Control.Effect.Sum
( -- * Membership
  Member(..)
, Members
  -- * Sums
, (:+:)(..)
, reassociateSumL
) where

import Data.Kind (Constraint, Type)

-- | Higher-order sums are used to combine multiple effects into a signature, typically by chaining on the right.
data (f :+: g) (m :: Type -> Type) k
  = L (f m k)
  | R (g m k)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infixr 4 :+:


-- | The class of types present in a signature.
--
--   This is based on Wouter Swierstra’s design described in [Data types à la carte](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf). As described therein, overlapping instances are required in order to distinguish e.g. left-occurrence from right-recursion.
--
--   It should not generally be necessary for you to define new 'Member' instances, but these are not specifically prohibited if you wish to get creative.
--
-- @since 0.1.0.0
class Member (sub :: (Type -> Type) -> (Type -> Type)) sup where
  -- | Inject a member of a signature into the signature.
  inj :: sub m a -> sup m a

-- | Reflexivity: @t@ is a member of itself.
instance Member t t where
  inj = id
  {-# INLINE inj #-}

-- | Left-recursion: if @t@ is a member of @l1 ':+:' l2 ':+:' r@, then we can inject it into @(l1 ':+:' l2) ':+:' r@ by injection into a right-recursive signature, followed by left-association.
instance {-# OVERLAPPABLE #-}
         Member t (l1 :+: l2 :+: r)
      => Member t ((l1 :+: l2) :+: r) where
  inj = reassociateSumL . inj
  {-# INLINE inj #-}

-- | Left-occurrence: if @t@ is at the head of a signature, we can inject it in O(1).
instance {-# OVERLAPPABLE #-}
         Member l (l :+: r) where
  inj = L
  {-# INLINE inj #-}

-- | Right-recursion: if @t@ is a member of @r@, we can inject it into @r@ in O(n), followed by lifting that into @l ':+:' r@ in O(1).
instance {-# OVERLAPPABLE #-}
         Member l r
      => Member l (l' :+: r) where
  inj = R . inj
  {-# INLINE inj #-}


-- | Reassociate a right-nested sum leftwards.
--
-- @since 1.0.2.0
reassociateSumL :: (l1 :+: l2 :+: r) m a -> ((l1 :+: l2) :+: r) m a
reassociateSumL = \case
  L l     -> L (L l)
  R (L l) -> L (R l)
  R (R r) -> R r
{-# INLINE reassociateSumL #-}


-- | Decompose sums on the left into multiple 'Member' constraints.
--
-- Note that while this, and by extension 'Control.Algebra.Has', can be used to group together multiple membership checks into a single (composite) constraint, large signatures on the left can slow compiles down due to [a problem with recursive type families](https://gitlab.haskell.org/ghc/ghc/issues/8095).
--
-- @since 1.0.0.0
type family Members sub sup :: Constraint where
  Members (l :+: r) u = (Members l u, Members r u)
  Members t         u = Member t u
