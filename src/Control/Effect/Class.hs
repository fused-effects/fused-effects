{-# LANGUAGE DefaultSignatures, EmptyCase, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeOperators #-}

-- | Provides the 'HFunctor' and 'Effect' classes that effect types implement.
--
-- @since 1.0.0.0
module Control.Effect.Class
( HFunctor(..)
, handleCoercible
, Effect(..)
-- * Generic deriving of 'HFunctor' & 'Effect' instances.
, GHFunctor(..)
, GEffect(..)
) where

import Data.Coerce
import GHC.Generics

-- | Higher-order functors of kind @(* -> *) -> (* -> *)@ map functors to functors.
--
--   All effects must be 'HFunctor's.
--
-- @since 1.0.0.0
class HFunctor h where
  -- | Higher-order functor map of a natural transformation over higher-order positions within the effect.
  --
  -- A definition for 'hmap' over first-order effects can be derived automatically provided a 'Generic1' instance is available.
  hmap :: Functor m => (forall x . m x -> n x) -> (h m a -> h n a)
  default hmap :: (Functor m, Generic1 (h m), Generic1 (h n), GHFunctor m n (Rep1 (h m)) (Rep1 (h n))) => (forall x . m x -> n x) -> (h m a -> h n a)
  hmap f = to1 . ghmap f . from1
  {-# INLINE hmap #-}


-- | Thread a 'Coercible' carrier through an 'HFunctor'.
--
--   This is applicable whenever @f@ is 'Coercible' to @g@, e.g. simple @newtype@s.
--
-- @since 1.0.0.0
handleCoercible :: (HFunctor sig, Functor f, Coercible f g) => sig f a -> sig g a
handleCoercible = hmap coerce
{-# INLINE handleCoercible #-}


-- | The class of effect types, which must:
--
--   1. Be functorial in their last two arguments, and
--   2. Support threading effects in higher-order positions through using the carrier’s suspended state.
--
-- All first-order effects (those without existential occurrences of @m@) admit a default definition of 'handle' provided a 'Generic1' instance is available for the effect.
--
-- @since 1.0.0.0
class HFunctor sig => Effect sig where
  -- | Handle any effects in a signature by threading the carrier’s state all the way through to the continuation.
  handle :: (Functor f, Monad m)
         => f ()
         -> (forall x . f (m x) -> n (f x))
         -> sig m a
         -> sig n (f a)
  default handle :: (Functor f, Monad m, Generic1 (sig m), Generic1 (sig n), GEffect m n (Rep1 (sig m)) (Rep1 (sig n)))
                 => f ()
                 -> (forall x . f (m x) -> n (f x))
                 -> sig m a
                 -> sig n (f a)
  handle state handler = to1 . ghandle state handler . from1
  {-# INLINE handle #-}


-- | Generic implementation of 'HFunctor'.
class GHFunctor m m' rep rep' where
  -- | Generic implementation of 'hmap'.
  ghmap :: Functor m => (forall x . m x -> m' x) -> (rep a -> rep' a)

instance GHFunctor m m' rep rep' => GHFunctor m m' (M1 i c rep) (M1 i c rep') where
  ghmap f = M1 . ghmap f . unM1
  {-# INLINE ghmap #-}

instance (GHFunctor m m' l l', GHFunctor m m' r r') => GHFunctor m m' (l :+: r) (l' :+: r') where
  ghmap f (L1 l) = L1 (ghmap f l)
  ghmap f (R1 r) = R1 (ghmap f r)
  {-# INLINE ghmap #-}

instance (GHFunctor m m' l l', GHFunctor m m' r r') => GHFunctor m m' (l :*: r) (l' :*: r') where
  ghmap f (l :*: r) = ghmap f l :*: ghmap f r
  {-# INLINE ghmap #-}

instance GHFunctor m m' V1 V1 where
  ghmap _ v = case v of {}
  {-# INLINE ghmap #-}

instance GHFunctor m m' U1 U1 where
  ghmap _ = id
  {-# INLINE ghmap #-}

instance GHFunctor m m' (K1 R c) (K1 R c) where
  ghmap _ = coerce
  {-# INLINE ghmap #-}

instance GHFunctor m m' Par1 Par1 where
  ghmap _ = coerce
  {-# INLINE ghmap #-}

instance (Functor f, GHFunctor m m' g g') => GHFunctor m m' (f :.: g) (f :.: g') where
  ghmap f = Comp1 . fmap (ghmap f) . unComp1
  {-# INLINE ghmap #-}

instance GHFunctor m m' (Rec1 m) (Rec1 m') where
  ghmap f = Rec1 . f . unRec1
  {-# INLINE ghmap #-}

instance HFunctor f => GHFunctor m m' (Rec1 (f m)) (Rec1 (f m')) where
  ghmap f = Rec1 . hmap f . unRec1
  {-# INLINE ghmap #-}


-- | Generic implementation of 'Effect'.
class GEffect m m' rep rep' where
  -- | Generic implementation of 'handle'.
  ghandle :: (Functor f, Monad m)
          => f ()
          -> (forall x . f (m x) -> m' (f x))
          -> rep a
          -> rep' (f a)

instance GEffect m m' rep rep' => GEffect m m' (M1 i c rep) (M1 i c rep') where
  ghandle state handler = M1 . ghandle state handler . unM1
  {-# INLINE ghandle #-}

instance (GEffect m m' l l', GEffect m m' r r') => GEffect m m' (l :+: r) (l' :+: r') where
  ghandle state handler (L1 l) = L1 (ghandle state handler l)
  ghandle state handler (R1 r) = R1 (ghandle state handler r)
  {-# INLINE ghandle #-}

instance (GEffect m m' l l', GEffect m m' r r') => GEffect m m' (l :*: r) (l' :*: r') where
  ghandle state handler (l :*: r) = ghandle state handler l :*: ghandle state handler r
  {-# INLINE ghandle #-}

instance GEffect m m' V1 V1 where
  ghandle _ _ v = case v of {}
  {-# INLINE ghandle #-}

instance GEffect m m' U1 U1 where
  ghandle _ _ = coerce
  {-# INLINE ghandle #-}

instance GEffect m m' (K1 R c) (K1 R c) where
  ghandle _ _ = coerce
  {-# INLINE ghandle #-}

instance GEffect m m' Par1 Par1 where
  ghandle state _ = Par1 . (<$ state) . unPar1
  {-# INLINE ghandle #-}

instance (Functor f, GEffect m m' g g') => GEffect m m' (f :.: g) (f :.: g') where
  ghandle state handler = Comp1 . fmap (ghandle state handler) . unComp1
  {-# INLINE ghandle #-}

instance GEffect m m' (Rec1 m) (Rec1 m') where
  ghandle state handler = Rec1 . handler . (<$ state) . unRec1
  {-# INLINE ghandle #-}

instance Effect f => GEffect m m' (Rec1 (f m)) (Rec1 (f m')) where
  ghandle state handler = Rec1 . handle state handler . unRec1
  {-# INLINE ghandle #-}
