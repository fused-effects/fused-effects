{-# LANGUAGE DefaultSignatures, DeriveFunctor, DeriveGeneric, EmptyCase, ExistentialQuantification, FlexibleContexts, FlexibleInstances, KindSignatures, MultiParamTypeClasses, RankNTypes, StandaloneDeriving, TypeOperators #-}

-- | Provides the 'Effect' class that effect types implement.
--
-- @since 1.0.0.0
module Control.Algebra.Internal
( -- * Effect class
  Effect(..)
, hmap
  -- * Effects
, Catch(..)
, Choose(..)
, Empty(..)
, Error
, Lift(..)
, NonDet
, Pure
, Reader(..)
, State(..)
, Throw(..)
, Writer(..)
  -- * Generic deriving of 'Effect' instances.
, GEffect(..)
) where

import qualified Control.Effect.Sum as Sum
import Data.Coerce
import Data.Functor.Identity
import GHC.Generics

-- Effect class

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



-- Effects

-- | 'Catch' effects can be used alongside 'Control.Effect.Throw.Throw' to provide recoverable exceptions.
--
-- @since 1.0.0.0
data Catch e m k
  = forall b . Catch (m b) (e -> m b) (b -> m k)

deriving instance Functor m => Functor (Catch e m)

instance Functor f => Effect f (Catch e) where
  handle state handler (Catch m h k) = Catch (handler (m <$ state)) (handler . (<$ state) . h) (handler . fmap k)

-- | @since 1.0.0.0
newtype Choose m k
  = Choose (Bool -> m k)
  deriving (Functor, Generic1)

instance Functor f => Effect f Choose

-- | @since 1.0.0.0
data Empty (m :: * -> *) k = Empty
  deriving (Functor, Generic1)

instance Functor f => Effect f Empty

-- | @since 0.1.0.0
type Error e = Throw e Sum.:+: Catch e

-- | @since 0.1.0.0
newtype Lift sig m k = Lift { unLift :: sig (m k) }
  deriving (Functor, Generic1)

instance (Functor f, Functor m) => Effect f (Lift m)

-- | The nondeterminism effect is the composition of 'Empty' and 'Choose' effects.
--
-- @since 0.1.0.0
type NonDet = Empty Sum.:+: Choose

-- | @since 0.3.0.0
data Pure (m :: * -> *) k
  deriving (Functor, Generic1)

instance Functor f => Effect f Pure

-- | @since 0.1.0.0
data Reader r m k
  = Ask (r -> m k)
  | forall b . Local (r -> r) (m b) (b -> m k)

deriving instance Functor m => Functor (Reader r m)

instance Functor f => Effect f (Reader r) where
  handle state handler (Ask k)       = Ask (handler . (<$ state) . k)
  handle state handler (Local f m k) = Local f (handler (m <$ state)) (handler . fmap k)

-- | @since 0.1.0.0
data State s m k
  = Get (s -> m k)
  | Put s (m k)
  deriving (Functor, Generic1)

instance Functor f => Effect f (State s)

-- | @since 1.0.0.0
data Throw e (m :: * -> *) k
  = Throw e
  deriving (Functor, Generic1)

instance Functor f => Effect f (Throw e)

-- | @since 0.1.0.0
data Writer w m k
  = Tell w (m k)
  | forall a . Listen (m a) (w -> a -> m k)
  | forall a . Censor (w -> w) (m a) (a -> m k)

deriving instance Functor m => Functor (Writer w m)

instance Functor f => Effect f (Writer w) where
  handle state handler (Tell w     k) = Tell w                          (handler (k <$ state))
  handle state handler (Listen   m k) = Listen   (handler (m <$ state)) (fmap handler . fmap . k)
  handle state handler (Censor f m k) = Censor f (handler (m <$ state)) (handler . fmap k)
  {-# INLINE handle #-}


instance (Effect f l, Effect f r) => Effect f (l Sum.:+: r)


-- Generic derivation

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
