{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Algebra.Dependent
( runDep
, Dep(Dep)
, DMember(..)
, DMembers
, DHas
, dsend
, module Control.Algebra
) where

import Control.Algebra
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Kind (Constraint)

newtype Dep (label :: k) (sub :: (* -> *) -> (* -> *)) m a = Dep { runDep :: sub m a }
  deriving (Applicative, Functor, HFunctor, Monad, MonadFail, MonadIO, MonadTrans)

instance Algebra sig (sub m) => Algebra sig (Dep label sub m) where
  alg = Dep . send . handleCoercible


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


-- | Decompose sums on the left into multiple 'Member' constraints.
--
-- Note that while this, and by extension 'DHas', can be used to group together multiple membership checks into a single (composite) constraint, large signatures on the left can slow compiles down due to [a problem with recursive type families](https://gitlab.haskell.org/ghc/ghc/issues/8095).
type family DMembers label sub sup = (res :: Constraint) | res -> label sub sup where
  DMembers label (l :+: r) u = (DMembers label l u, DMembers label r u)
  DMembers label t         u = DMember label t u


-- | @m@ is a carrier for @sig@ containing @eff@ associated with @label@.
--
-- Note that if @eff@ is a sum, it will be decomposed into multiple 'DMember' constraints. While this technically allows one to combine multiple unrelated effects into a single 'DHas' constraint, doing so has two significant drawbacks:
--
-- 1. Due to [a problem with recursive type families](https://gitlab.haskell.org/ghc/ghc/issues/8095), this can lead to significantly slower compiles.
--
-- 2. It defeats @ghc@’s warnings for redundant constraints, and thus can lead to a proliferation of redundant constraints as code is changed.
type DHas label eff sig m = (DMembers label eff sig, Algebra sig m)


-- | Construct a request for an effect to be interpreted by some handler later on.
dsend :: (DMember label eff sig, Algebra sig m) => Dep label eff m a -> m a
dsend = alg . dinj
{-# INLINE dsend #-}
