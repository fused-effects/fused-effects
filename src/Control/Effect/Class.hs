{-# LANGUAGE DefaultSignatures, EmptyCase, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeOperators #-}

-- | Provides the 'Effect' class that effect types implement.
--
-- @since 1.0.0.0
module Control.Effect.Class
( -- * 'Effect' class
  Effect(..)
  -- * Generic deriving of 'Effect' instances.
, GEffect(..)
) where

import Data.Coerce
import GHC.Generics

-- | The class of effect types, which must:
--
--   1. Be functorial in their last two arguments, and
--   2. Support threading effects in higher-order positions through using the carrier’s suspended context.
--
-- All first-order effects (those without existential occurrences of @m@) admit a default definition of 'handle' provided a 'Generic1' instance is available for the effect.
--
-- @since 1.0.0.0
class Effect sig where
  -- | Handle any effects in a signature by threading the carrier’s context all the way through to the continuation.
  handle :: (Functor ctx, Monad m)
         => ctx ()
         -> (forall x . ctx (m x) -> n (ctx x))
         -> sig m a
         -> sig n (ctx a)
  default handle :: (Functor ctx, Monad m, Generic1 (sig m), Generic1 (sig n), GEffect m n (Rep1 (sig m)) (Rep1 (sig n)))
                 => ctx ()
                 -> (forall x . ctx (m x) -> n (ctx x))
                 -> sig m a
                 -> sig n (ctx a)
  handle ctx handler = to1 . ghandle ctx handler . from1
  {-# INLINE handle #-}


-- | Generic implementation of 'Effect'.
class GEffect m m' rep rep' where
  -- | Generic implementation of 'handle'.
  ghandle :: (Functor ctx, Monad m)
          => ctx ()
          -> (forall x . ctx (m x) -> m' (ctx x))
          -> rep a
          -> rep' (ctx a)

instance GEffect m m' rep rep' => GEffect m m' (M1 i c rep) (M1 i c rep') where
  ghandle ctx handler = M1 . ghandle ctx handler . unM1
  {-# INLINE ghandle #-}

instance (GEffect m m' l l', GEffect m m' r r') => GEffect m m' (l :+: r) (l' :+: r') where
  ghandle ctx handler (L1 l) = L1 (ghandle ctx handler l)
  ghandle ctx handler (R1 r) = R1 (ghandle ctx handler r)
  {-# INLINE ghandle #-}

instance (GEffect m m' l l', GEffect m m' r r') => GEffect m m' (l :*: r) (l' :*: r') where
  ghandle ctx handler (l :*: r) = ghandle ctx handler l :*: ghandle ctx handler r
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
  ghandle ctx _ = Par1 . (<$ ctx) . unPar1
  {-# INLINE ghandle #-}

instance (Functor f, GEffect m m' g g') => GEffect m m' (f :.: g) (f :.: g') where
  ghandle ctx handler = Comp1 . fmap (ghandle ctx handler) . unComp1
  {-# INLINE ghandle #-}

instance GEffect m m' (Rec1 m) (Rec1 m') where
  ghandle ctx handler = Rec1 . handler . (<$ ctx) . unRec1
  {-# INLINE ghandle #-}

instance Effect f => GEffect m m' (Rec1 (f m)) (Rec1 (f m')) where
  ghandle ctx handler = Rec1 . handle ctx handler . unRec1
  {-# INLINE ghandle #-}
