{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Control.Effect.Close
import Data.Proxy


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

instance Subsume (Elem sub sup) sub sup => Member sub sup where
  inj = iinj (Proxy @(Elem sub sup))
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
