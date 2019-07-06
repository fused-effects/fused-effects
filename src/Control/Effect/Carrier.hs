{-# LANGUAGE DefaultSignatures, DeriveFunctor, FlexibleContexts, FlexibleInstances, FunctionalDependencies, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Effect.Carrier
( HFunctor(..)
, Effect(..)
, Carrier(..)
, handlePure
, handleCoercible
) where

import Data.Coerce
import GHC.Generics

class HFunctor h where
  -- | Higher-order functor map of a natural transformation over higher-order positions within the effect.
  hmap :: (Functor m, Functor n) => (forall x . m x -> n x) -> (h m a -> h n a)
  default hmap :: (Functor m, Functor n, Generic1 (h m), Generic1 (h n), GHFunctor m n (Rep1 (h m)) (Rep1 (h n))) => (forall x . m x -> n x) -> (h m a -> h n a)
  hmap f = to1 . ghmap f . from1
  {-# INLINE hmap #-}


-- | The class of effect types, which must:
--
--   1. Be functorial in their last two arguments, and
--   2. Support threading effects in higher-order positions through using the carrier’s suspended state.
class HFunctor sig => Effect sig where
  -- | Handle any effects in a signature by threading the carrier’s state all the way through to the continuation.
  handle :: (Functor f, Monad m, Monad n)
         => f ()
         -> (forall x . f (m x) -> n (f x))
         -> sig m a
         -> sig n (f a)


-- | The class of carriers (results) for algebras (effect handlers) over signatures (effects), whose actions are given by the 'eff' method.
class (HFunctor sig, Monad m) => Carrier sig m | m -> sig where
  -- | Construct a value in the carrier for an effect signature (typically a sum of a handled effect and any remaining effects).
  eff :: sig m a -> m a

-- | Apply a handler specified as a natural transformation to both higher-order and continuation positions within an 'HFunctor'.
handlePure :: (HFunctor sig, Functor f, Functor g) => (forall x . f x -> g x) -> sig f a -> sig g a
handlePure handler = hmap handler
{-# INLINE handlePure #-}

-- | Thread a 'Coercible' carrier through an 'HFunctor'.
--
--   This is applicable whenever @f@ is 'Coercible' to @g@, e.g. simple @newtype@s.
handleCoercible :: (HFunctor sig, Functor f, Functor g, Coercible f g) => sig f a -> sig g a
handleCoercible = handlePure coerce
{-# INLINE handleCoercible #-}



class GHFunctor m m' rep rep' where
  ghmap :: (Functor m, Functor m') => (forall x . m x -> m' x) -> (rep a -> rep' a)

instance GHFunctor m m' rep rep' => GHFunctor m m' (M1 i c rep) (M1 i c rep') where
  ghmap f = M1 . ghmap f . unM1

instance (GHFunctor m m' l l', GHFunctor m m' r r') => GHFunctor m m' (l :+: r) (l' :+: r') where
  ghmap f (L1 l) = L1 (ghmap f l)
  ghmap f (R1 r) = R1 (ghmap f r)

instance (GHFunctor m m' l l', GHFunctor m m' r r') => GHFunctor m m' (l :*: r) (l' :*: r') where
  ghmap f (l :*: r) = ghmap f l :*: ghmap f r

instance GHFunctor m m' (K1 R c) (K1 R c) where
  ghmap _ = coerce

instance (Functor f, GHFunctor m m' g g') => GHFunctor m m' (f :.: g) (f :.: g') where
  ghmap f = Comp1 . fmap (ghmap f) . unComp1

instance GHFunctor m m' (Rec1 m) (Rec1 m') where
  ghmap f = Rec1 . f . unRec1
