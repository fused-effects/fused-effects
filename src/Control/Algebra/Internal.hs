{-# LANGUAGE DefaultSignatures, DeriveFunctor, DeriveGeneric, EmptyCase, ExistentialQuantification, FlexibleContexts, FlexibleInstances, FunctionalDependencies, KindSignatures, RankNTypes, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Provides the 'Effect' class that effect types implement.
--
-- @since 1.0.0.0
module Control.Algebra.Internal
( -- * Algebra class
  Algebra(..)
, handleIdentity
, handleCoercible
  -- * Effect class
, Effect(..)
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

import Control.Effect.Sum as Sum
import Control.Monad ((<=<), join)
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Identity as Identity
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.RWS.Lazy as RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict as RWS.Strict
import qualified Control.Monad.Trans.State.Lazy as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict
import qualified Control.Monad.Trans.Writer.Lazy as Writer.Lazy
import qualified Control.Monad.Trans.Writer.Strict as Writer.Strict
import Data.Coerce
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Semigroup as S
import Data.Tuple (swap)
import GHC.Generics as Generics

-- Algebra class

-- | The class of carriers (results) for algebras (effect handlers) over signatures (effects), whose actions are given by the 'alg' method.
--
-- @since 1.0.0.0
class Monad m => Algebra sig m | m -> sig where
  type Suspend m :: * -> *
  type Suspend m = Identity

  -- | Construct a value in the carrier for an effect signature (typically a sum of a handled effect and any remaining effects).
  alg :: sig m a -> m a

handleIdentity :: (Monad m, Effect Identity eff, Member eff sig, Algebra sig n) => (forall x . m x -> n x) -> eff m a -> n a
handleIdentity f = fmap runIdentity . alg . inj . handle (Identity ()) (fmap Identity . f . runIdentity)
{-# INLINE handleIdentity #-}

-- | Thread a 'Coercible' carrier through an 'Effect'.
--
--   This is applicable whenever @m@ is 'Coercible' to @n@, e.g. simple @newtype@s.
--
-- @since 1.0.0.0
handleCoercible :: (Monad m, Effect Identity eff, Member eff sig, Algebra sig n, Coercible m n) => eff m a -> n a
handleCoercible = handleIdentity coerce
{-# INLINE handleCoercible #-}



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


-- Algebra instances

-- base

instance Algebra (Lift IO) IO where
  alg = join . unLift

instance Algebra Pure Identity where
  alg v = case v of {}

instance Algebra Choose NonEmpty where
  alg (Choose m) = m True S.<> m False

instance Algebra Empty Maybe where
  alg Empty = Nothing

instance Algebra (Error e) (Either e) where
  alg (L (Throw e))     = Left e
  alg (R (Catch m h k)) = either (k <=< h) k m

instance Algebra (Reader r) ((->) r) where
  alg (Ask       k) r = k r r
  alg (Local f m k) r = k (m (f r)) r

instance Algebra NonDet [] where
  alg (L Empty)      = []
  alg (R (Choose k)) = k True ++ k False

instance Monoid w => Algebra (Writer w) ((,) w) where
  alg (Tell w (w', k))    = (mappend w w', k)
  alg (Listen (w, a) k)   = let (w', a') = k w a in (mappend w w', a')
  alg (Censor f (w, a) k) = let (w', a') = k a in (mappend (f w) w', a')


-- transformers

instance (Algebra sig m, Effect (Either e) sig) => Algebra (Error e Sum.:+: sig) (Except.ExceptT e m) where
  type Suspend (Except.ExceptT e m) = Either e
  alg (L (L (Throw e)))     = Except.throwE e
  alg (L (R (Catch m h k))) = Except.catchE m h >>= k
  alg (R other)             = Except.ExceptT $ alg (handle (Right ()) (either (pure . Left) Except.runExceptT) other)

instance (Algebra sig m, Effect Identity sig) => Algebra sig (Identity.IdentityT m) where
  alg = Identity.IdentityT . handleCoercible

instance (Algebra sig m, Effect Identity sig) => Algebra (Reader r Sum.:+: sig) (Reader.ReaderT r m) where
  alg (L (Ask       k)) = Reader.ask >>= k
  alg (L (Local f m k)) = Reader.local f m >>= k
  alg (R other)         = Reader.ReaderT $ \ r -> handleIdentity (flip Reader.runReaderT r) other

newtype RWSTF w s a = RWSTF { unRWSTF :: (a, s, w) }
  deriving (Functor)

toRWSTF :: Monoid w => w -> (a, s, w) -> RWSTF w s a
toRWSTF w (a, s, w') = RWSTF (a, s, mappend w w')
{-# INLINE toRWSTF #-}

instance (Algebra sig m, Effect (RWSTF w s) sig, Monoid w) => Algebra (Reader r Sum.:+: Writer w Sum.:+: State s Sum.:+: sig) (RWS.Lazy.RWST r w s m) where
  type Suspend (RWS.Lazy.RWST r w s m) = RWSTF w s
  alg (L (Ask       k))      = RWS.Lazy.ask >>= k
  alg (L (Local f m k))      = RWS.Lazy.local f m >>= k
  alg (R (L (Tell w k)))     = RWS.Lazy.tell w *> k
  alg (R (L (Listen m k)))   = RWS.Lazy.listen m >>= uncurry (flip k)
  alg (R (L (Censor f m k))) = RWS.Lazy.censor f m >>= k
  alg (R (R (L (Get   k))))  = RWS.Lazy.get >>= k
  alg (R (R (L (Put s k))))  = RWS.Lazy.put s *> k
  alg (R (R (R other)))      = RWS.Lazy.RWST $ \ r s -> unRWSTF <$> alg (handle (RWSTF ((), s, mempty)) (\ (RWSTF (x, s, w)) -> toRWSTF w <$> RWS.Lazy.runRWST x r s) other)

instance (Algebra sig m, Effect (RWSTF w s) sig, Monoid w) => Algebra (Reader r Sum.:+: Writer w Sum.:+: State s Sum.:+: sig) (RWS.Strict.RWST r w s m) where
  type Suspend (RWS.Strict.RWST r w s m) = RWSTF w s
  alg (L (Ask       k))      = RWS.Strict.ask >>= k
  alg (L (Local f m k))      = RWS.Strict.local f m >>= k
  alg (R (L (Tell w k)))     = RWS.Strict.tell w *> k
  alg (R (L (Listen m k)))   = RWS.Strict.listen m >>= uncurry (flip k)
  alg (R (L (Censor f m k))) = RWS.Strict.censor f m >>= k
  alg (R (R (L (Get   k))))  = RWS.Strict.get >>= k
  alg (R (R (L (Put s k))))  = RWS.Strict.put s *> k
  alg (R (R (R other)))      = RWS.Strict.RWST $ \ r s -> unRWSTF <$> alg (handle (RWSTF ((), s, mempty)) (\ (RWSTF (x, s, w)) -> toRWSTF w <$> RWS.Strict.runRWST x r s) other)

instance (Algebra sig m, Effect ((,) s) sig) => Algebra (State s Sum.:+: sig) (State.Lazy.StateT s m) where
  type Suspend (State.Lazy.StateT s m) = (,) s
  alg (L (Get   k)) = State.Lazy.get >>= k
  alg (L (Put s k)) = State.Lazy.put s *> k
  alg (R other)     = State.Lazy.StateT $ \ s -> swap <$> alg (handle (s, ()) (\ (s, x) -> swap <$> State.Lazy.runStateT x s) other)

instance (Algebra sig m, Effect ((,) s) sig) => Algebra (State s Sum.:+: sig) (State.Strict.StateT s m) where
  type Suspend (State.Strict.StateT s m) = (,) s
  alg (L (Get   k)) = State.Strict.get >>= k
  alg (L (Put s k)) = State.Strict.put s *> k
  alg (R other)     = State.Strict.StateT $ \ s -> swap <$> alg (handle (s, ()) (\ (s, x) -> swap <$> State.Strict.runStateT x s) other)

instance (Algebra sig m, Effect ((,) w) sig, Monoid w) => Algebra (Writer w Sum.:+: sig) (Writer.Lazy.WriterT w m) where
  type Suspend (Writer.Lazy.WriterT w m) = (,) w
  alg (L (Tell w k))     = Writer.Lazy.tell w *> k
  alg (L (Listen m k))   = Writer.Lazy.listen m >>= uncurry (flip k)
  alg (L (Censor f m k)) = Writer.Lazy.censor f m >>= k
  alg (R other)          = Writer.Lazy.WriterT $ swap <$> alg (handle (mempty, ()) (\ (s, x) -> swap . fmap (mappend s) <$> Writer.Lazy.runWriterT x) other)

instance (Algebra sig m, Effect ((,) w) sig, Monoid w) => Algebra (Writer w Sum.:+: sig) (Writer.Strict.WriterT w m) where
  type Suspend (Writer.Strict.WriterT w m) = (,) w
  alg (L (Tell w k))     = Writer.Strict.tell w *> k
  alg (L (Listen m k))   = Writer.Strict.listen m >>= uncurry (flip k)
  alg (L (Censor f m k)) = Writer.Strict.censor f m >>= k
  alg (R other)          = Writer.Strict.WriterT $ swap <$> alg (handle (mempty, ()) (\ (s, x) -> swap . fmap (mappend s) <$> Writer.Strict.runWriterT x) other)


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

instance (GEffect f m m' l l', GEffect f m m' r r') => GEffect f m m' (l Generics.:+: r) (l' Generics.:+: r') where
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
