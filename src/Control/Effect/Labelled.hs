{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Effect.Labelled
( runLabelled
, Labelled(Labelled)
, LabelledMember(..)
, LabelledMembers
, HasLabelled
, dsend
, runInLabel
, InLabel(InLabel)
, module Control.Algebra
) where

import Control.Algebra
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Kind (Constraint)

newtype Labelled (label :: k) (sub :: (* -> *) -> (* -> *)) m a = Labelled { runLabelled :: sub m a }
  deriving (Applicative, Effect, Functor, HFunctor, Monad, MonadFail, MonadIO, MonadTrans)

instance (Algebra (eff :+: sig) (sub m), HFunctor eff, HFunctor sig) => Algebra (Labelled label eff :+: sig) (Labelled label sub m) where
  alg = \case
    L eff -> Labelled (send (handleCoercible (runLabelled eff)))
    R sig -> Labelled (send (handleCoercible sig))


class LabelledMember label (sub :: (* -> *) -> (* -> *)) sup | label sup -> sub where
  -- | Inject a member of a signature into the signature.
  dinj :: Labelled label sub m a -> sup m a

-- | Reflexivity: @t@ is a member of itself.
instance LabelledMember label t (Labelled label t) where
  dinj = id

-- | Left-recursion: if @t@ is a member of @l1 ':+:' l2 ':+:' r@, then we can inject it into @(l1 ':+:' l2) ':+:' r@ by injection into a right-recursive signature, followed by left-association.
instance {-# OVERLAPPABLE #-}
         LabelledMember label t (l1 :+: l2 :+: r)
      => LabelledMember label t ((l1 :+: l2) :+: r) where
  dinj = reassoc . dinj where
    reassoc (L l)     = L (L l)
    reassoc (R (L l)) = L (R l)
    reassoc (R (R r)) = R r

-- | Left-occurrence: if @t@ is at the head of a signature, we can inject it in O(1).
instance {-# OVERLAPPABLE #-}
         LabelledMember label l (Labelled label l :+: r) where
  dinj = L

-- | Right-recursion: if @t@ is a member of @r@, we can inject it into @r@ in O(n), followed by lifting that into @l ':+:' r@ in O(1).
instance {-# OVERLAPPABLE #-}
         LabelledMember label l r
      => LabelledMember label l (l' :+: r) where
  dinj = R . dinj


-- | Decompose sums on the left into multiple 'LabelledMember' constraints.
--
-- Note that while this, and by extension 'HasLabelled', can be used to group together multiple membership checks into a single (composite) constraint, large signatures on the left can slow compiles down due to [a problem with recursive type families](https://gitlab.haskell.org/ghc/ghc/issues/8095).
type family LabelledMembers label sub sup = (res :: Constraint) | res -> label sub sup where
  LabelledMembers label (l :+: r) u = (LabelledMembers label l u, LabelledMembers label r u)
  LabelledMembers label t         u = LabelledMember label t u


-- | @m@ is a carrier for @sig@ containing @eff@ associated with @label@.
--
-- Note that if @eff@ is a sum, it will be decomposed into multiple 'LabelledMember' constraints. While this technically allows one to combine multiple unrelated effects into a single 'HasLabelled' constraint, doing so has two significant drawbacks:
--
-- 1. Due to [a problem with recursive type families](https://gitlab.haskell.org/ghc/ghc/issues/8095), this can lead to significantly slower compiles.
--
-- 2. It defeats @ghc@’s warnings for redundant constraints, and thus can lead to a proliferation of redundant constraints as code is changed.
type HasLabelled label eff sig m = (LabelledMembers label eff sig, Algebra sig m)


-- | Construct a request for an effect to be interpreted by some handler later on.
dsend :: (LabelledMember label eff sig, Algebra sig m) => Labelled label eff m a -> m a
dsend = alg . dinj
{-# INLINE dsend #-}


newtype InLabel (label :: k) (sub :: (* -> *) -> (* -> *)) (m :: * -> *) a = InLabel { runInLabel :: m a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance MonadTrans (InLabel sub label) where
  lift = InLabel

instance (LabelledMember label sub sig, HFunctor sub, Algebra sig m) => Algebra (sub :+: sig) (InLabel label sub m) where
  alg = \case
    L sub -> InLabel (dsend @label (Labelled (handleCoercible sub)))
    R sig -> InLabel (send (handleCoercible sig))
