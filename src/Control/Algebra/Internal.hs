{-# LANGUAGE DefaultSignatures, DeriveFunctor, DeriveGeneric, EmptyCase, FlexibleContexts, FlexibleInstances, KindSignatures, MultiParamTypeClasses, RankNTypes, TypeOperators #-}

-- | Provides the 'Effect' class that effect types implement.
--
-- @since 1.0.0.0
module Control.Algebra.Internal
( Effect(..)
, hmap
  -- * Effects
, Choose(..)
, Empty(..)
  -- * Generic deriving of 'Effect' instances.
, GEffect(..)
) where

import qualified Control.Effect.Sum as Sum
import Data.Coerce
import Data.Functor.Identity
import GHC.Generics

-- | The class of effect types, which must:
--
--   1. Be functorial in their last two arguments, and
--   2. Support threading effects in higher-order positions through using the carrier’s suspended state.
--
-- All first-order effects (those without existential occurrences of @m@) admit a default definition of 'handle' provided a 'Generic1' instance is available for the effect.
--
-- @since 1.0.0.0
class Functor f => Effect f sig where
  -- | Handle any effects in a signature by threading the carrier’s state all the way through to the continuation.
  handle
    :: Monad m
    => f ()
    -> (forall x . f (m x) -> n (f x))
    -> sig m a
    -> sig n (f a)
  default handle
    :: (Monad m, Generic1 (sig m), Generic1 (sig n), GEffect f m n (Rep1 (sig m)) (Rep1 (sig n)))
    => f ()
    -> (forall x . f (m x) -> n (f x))
    -> sig m a
    -> sig n (f a)
  handle state handler = to1 . ghandle state handler . from1
  {-# INLINE handle #-}

-- | Higher-order functor map of a natural transformation over higher-order positions within the effect.
--
-- @since 1.0.0.0
hmap :: (Effect Identity sig, Functor n, Functor (sig n), Monad m) => (forall x . m x -> n x) -> (sig m a -> sig n a)
hmap f = fmap runIdentity . handle (Identity ()) (fmap Identity . f . runIdentity)
{-# INLINE hmap #-}


-- | @since 1.0.0.0
newtype Choose m k
  = Choose (Bool -> m k)
  deriving (Functor, Generic1)

instance Functor f => Effect f Choose

-- | @since 1.0.0.0
data Empty (m :: * -> *) k = Empty
  deriving (Functor, Generic1)

instance Functor f => Effect f Empty

instance (Effect f l, Effect f r) => Effect f (l Sum.:+: r)


-- | Generic implementation of 'Effect'.
class GEffect f m m' rep rep' where
  -- | Generic implementation of 'handle'.
  ghandle
    :: Monad m
    => f ()
    -> (forall x . f (m x) -> m' (f x))
    -> rep a
    -> rep' (f a)

instance GEffect f m m' rep rep' => GEffect f m m' (M1 i c rep) (M1 i c rep') where
  ghandle state handler = M1 . ghandle state handler . unM1
  {-# INLINE ghandle #-}

instance (GEffect f m m' l l', GEffect f m m' r r') => GEffect f m m' (l :+: r) (l' :+: r') where
  ghandle state handler (L1 l) = L1 (ghandle state handler l)
  ghandle state handler (R1 r) = R1 (ghandle state handler r)
  {-# INLINE ghandle #-}

instance (GEffect f m m' l l', GEffect f m m' r r') => GEffect f m m' (l :*: r) (l' :*: r') where
  ghandle state handler (l :*: r) = ghandle state handler l :*: ghandle state handler r
  {-# INLINE ghandle #-}

instance GEffect f m m' V1 V1 where
  ghandle _ _ v = case v of {}
  {-# INLINE ghandle #-}

instance GEffect f m m' U1 U1 where
  ghandle _ _ = coerce
  {-# INLINE ghandle #-}

instance GEffect f m m' (K1 R c) (K1 R c) where
  ghandle _ _ = coerce
  {-# INLINE ghandle #-}

instance Functor f => GEffect f m m' Par1 Par1 where
  ghandle state _ = Par1 . (<$ state) . unPar1
  {-# INLINE ghandle #-}

instance (Functor g, GEffect f m m' h h') => GEffect f m m' (g :.: h) (g :.: h') where
  ghandle state handler = Comp1 . fmap (ghandle state handler) . unComp1
  {-# INLINE ghandle #-}

instance Functor f => GEffect f m m' (Rec1 m) (Rec1 m') where
  ghandle state handler = Rec1 . handler . (<$ state) . unRec1
  {-# INLINE ghandle #-}

instance Effect f sig => GEffect f m m' (Rec1 (sig m)) (Rec1 (sig m')) where
  ghandle state handler = Rec1 . handle state handler . unRec1
  {-# INLINE ghandle #-}
