{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Algebra.Dependent
( Dep(..)
) where

import Control.Effect.Sum

newtype Dep (label :: k) (eff :: (* -> *) -> (* -> *)) m a = Dep { getDep :: eff m a }

class DMember label (sub :: (* -> *) -> (* -> *)) sup | label sup -> sub where
  -- | Inject a member of a signature into the signature.
  dinj :: Dep label sub m a -> sup m a

-- | Reflexivity: @t@ is a member of itself.
instance DMember label t (Dep label t) where
  dinj = id

-- | Left-recursion: if @t@ is a member of @l1 ':+:' l2 ':+:' r@, then we can inject it into @(l1 ':+:' l2) ':+:' r@ by injection into a right-recursive signature, followed by left-association.
instance {-# OVERLAPPABLE #-}
         DMember label t (l1 :+: l2 :+: r)
      => DMember label t ((l1 :+: l2) :+: r) where
  dinj = reassoc . dinj where
    reassoc (L l)     = L (L l)
    reassoc (R (L l)) = L (R l)
    reassoc (R (R r)) = R r

-- | Left-occurrence: if @t@ is at the head of a signature, we can inject it in O(1).
instance {-# OVERLAPPABLE #-}
         DMember label l (Dep label l :+: r) where
  dinj = L

-- | Right-recursion: if @t@ is a member of @r@, we can inject it into @r@ in O(n), followed by lifting that into @l ':+:' r@ in O(1).
instance {-# OVERLAPPABLE #-}
         DMember label l r
      => DMember label l (l' :+: r) where
  dinj = R . dinj
