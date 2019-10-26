{-# LANGUAGE ConstraintKinds, DefaultSignatures, DeriveFunctor, EmptyCase, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, RankNTypes, TypeFamilies, TypeOperators, UndecidableInstances, UndecidableSuperClasses #-}

{- | The 'Algebra' class is the mechanism with which effects are interpreted.

An instance of the 'Algebra' class defines an interpretation of an effect signature atop a given monad.

@since 1.0.0.0
-}
module Control.Algebra
( Algebra(..)
, AlgebraTrans(..)
, algDefault
, MonadTransContext(..)
, handling
, Has
, send
, handle
, handleIdentity
, handleCoercible
  -- * Re-exports
, (:+:) (..)
, run
, module Control.Effect.Class
) where

import Control.Carrier.Pure (PureC, run)
import Control.Effect.Catch.Internal
import Control.Effect.Choose.Internal
import Control.Effect.Class
import Control.Effect.Empty.Internal
import Control.Effect.Error.Internal
import Control.Effect.Lift.Internal
import Control.Effect.NonDet.Internal
import Control.Effect.Pure
import Control.Effect.Reader.Internal
import Control.Effect.State.Internal
import Control.Effect.Sum ((:+:)(..), Member(..), Members)
import Control.Effect.Throw.Internal
import Control.Effect.Writer.Internal
import Control.Monad ((<=<), join)
import Data.Functor.Identity
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Identity as Identity
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.RWS.Lazy as RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict as RWS.Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Data.Coerce
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Semigroup as S
import Data.Tuple (swap)

-- | The class of carriers (results) for algebras (effect handlers) over signatures (effects), whose actions are given by the 'alg' method.
--
-- @since 1.0.0.0
class (Effect sig, Monad m) => Algebra sig m | m -> sig where
  -- | Construct a value in the carrier for an effect signature (typically a sum of a handled effect and any remaining effects).
  alg :: sig m a -> m a
  default alg
    :: (AlgebraTrans eff t, CanThread sig' (Context t), Algebra sig' n, m ~ t n, sig ~ (eff :+: sig'))
    => sig m a
    -> m a
  alg (L l) = liftAlg l
  alg (R r) = liftHandle (\ ctx dst -> alg (thread ctx dst r))

algDefault
  :: (AlgebraTrans eff t, CanThread sig (Context t), Monad (t m), Algebra sig m)
  => (eff :+: sig) (t m) a
  -> t m a
algDefault (L l) = liftAlg l
algDefault (R r) = liftHandle (\ ctx dst -> alg (thread ctx dst r))


class MonadTransContext t => AlgebraTrans eff t | t -> eff where
  liftAlg :: (Algebra sig m, CanThread sig (Context t)) => eff (t m) a -> t m a

instance Algebra Pure PureC where
  alg v = case v of {}
  {-# INLINE alg #-}


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

instance (Algebra sig m, CanThread sig (Either e)) => Algebra (Error e :+: sig) (Except.ExceptT e m)

instance AlgebraTrans (Error e) (Except.ExceptT e) where
  liftAlg (L (Throw e))     = Except.throwE e
  liftAlg (R (Catch m h k)) = Except.catchE m h >>= k

instance Algebra sig m => Algebra (Reader r :+: sig) (Reader.ReaderT r m)

instance AlgebraTrans (Reader r) (Reader.ReaderT r) where
  liftAlg (Ask       k) = Reader.ask >>= k
  liftAlg (Local f m k) = Reader.local f m >>= k

newtype RWSTF w s a = RWSTF { unRWSTF :: (a, s, w) }
  deriving (Functor)

toRWSTF :: Monoid w => w -> (a, s, w) -> RWSTF w s a
toRWSTF w (a, s, w') = RWSTF (a, s, mappend w w')
{-# INLINE toRWSTF #-}

instance (Monoid w, Algebra sig m, CanThread sig (RWSTF w s)) => Algebra ((Reader r :+: Writer w :+: State s) :+: sig) (RWS.Lazy.RWST r w s m)

instance Monoid w => AlgebraTrans (Reader r :+: Writer w :+: State s) (RWS.Lazy.RWST r w s) where
  liftAlg (L (Ask       k))      = RWS.Lazy.ask >>= k
  liftAlg (L (Local f m k))      = RWS.Lazy.local f m >>= k
  liftAlg (R (L (Tell w k)))     = RWS.Lazy.tell w *> k
  liftAlg (R (L (Listen m k)))   = RWS.Lazy.listen m >>= uncurry (flip k)
  liftAlg (R (L (Censor f m k))) = RWS.Lazy.censor f m >>= k
  liftAlg (R (R (Get   k)))      = RWS.Lazy.get >>= k
  liftAlg (R (R (Put s k)))      = RWS.Lazy.put s *> k

instance (Monoid w, Algebra sig m, CanThread sig (RWSTF w s)) => Algebra ((Reader r :+: Writer w :+: State s) :+: sig) (RWS.Strict.RWST r w s m)

instance Monoid w => AlgebraTrans (Reader r :+: Writer w :+: State s) (RWS.Strict.RWST r w s) where
  liftAlg (L (Ask       k))      = RWS.Strict.ask >>= k
  liftAlg (L (Local f m k))      = RWS.Strict.local f m >>= k
  liftAlg (R (L (Tell w k)))     = RWS.Strict.tell w *> k
  liftAlg (R (L (Listen m k)))   = RWS.Strict.listen m >>= uncurry (flip k)
  liftAlg (R (L (Censor f m k))) = RWS.Strict.censor f m >>= k
  liftAlg (R (R (Get   k)))      = RWS.Strict.get >>= k
  liftAlg (R (R (Put s k)))      = RWS.Strict.put s *> k

instance (Algebra sig m, CanThread sig ((,) s)) => Algebra (State s :+: sig) (Lazy.StateT s m)

instance AlgebraTrans (State s) (Lazy.StateT s) where
  liftAlg (Get   k) = Lazy.get >>= k
  liftAlg (Put s k) = Lazy.put s *> k

instance (Algebra sig m, CanThread sig ((,) s)) => Algebra (State s :+: sig) (Strict.StateT s m)

instance AlgebraTrans (State s) (Strict.StateT s) where
  liftAlg (Get   k) = Strict.get >>= k
  liftAlg (Put s k) = Strict.put s *> k

instance (Monoid w, Algebra sig m, CanThread sig ((,) w)) => Algebra (Writer w :+: sig) (Lazy.WriterT w m)

instance Monoid w => AlgebraTrans (Writer w) (Lazy.WriterT w) where
  liftAlg (Tell w k)     = Lazy.tell w *> k
  liftAlg (Listen m k)   = Lazy.listen m >>= uncurry (flip k)
  liftAlg (Censor f m k) = Lazy.censor f m >>= k

instance (Monoid w, Algebra sig m, CanThread sig ((,) w)) => Algebra (Writer w :+: sig) (Strict.WriterT w m)

instance Monoid w => AlgebraTrans (Writer w) (Strict.WriterT w) where
  liftAlg (Tell w k)     = Strict.tell w *> k
  liftAlg (Listen m k)   = Strict.listen m >>= uncurry (flip k)
  liftAlg (Censor f m k) = Strict.censor f m >>= k


class MonadTrans t => MonadTransContext t where
  type Context t :: * -> *
  type Context t = Identity
  liftHandle :: Monad m => (Context t () -> (forall a . Context t (t m a) -> m (Context t a)) -> m (Context t a)) -> t m a

instance MonadTransContext (Except.ExceptT e) where
  type Context (Except.ExceptT e) = Either e
  liftHandle handle = Except.ExceptT (handle (Right ()) (either (pure . Left) Except.runExceptT))

instance MonadTransContext Identity.IdentityT where
  liftHandle handle = Identity.IdentityT (liftIdentity handle Identity.runIdentityT)

instance MonadTransContext Maybe.MaybeT where
  type Context Maybe.MaybeT = Maybe
  liftHandle handle = Maybe.MaybeT (handle (Just ()) (maybe (pure Nothing) Maybe.runMaybeT))

instance MonadTransContext (Reader.ReaderT r) where
  liftHandle handle = Reader.ReaderT (\ r -> liftIdentity handle (flip Reader.runReaderT r))

instance Monoid w => MonadTransContext (RWS.Lazy.RWST r w s) where
  type Context (RWS.Lazy.RWST r w s) = RWSTF w s
  liftHandle handle = RWS.Lazy.RWST (\ r s -> unRWSTF <$> handle (RWSTF ((), s, mempty)) (\ (RWSTF (x, s, w)) -> toRWSTF w <$> RWS.Lazy.runRWST x r s))

instance Monoid w => MonadTransContext (RWS.Strict.RWST r w s) where
  type Context (RWS.Strict.RWST r w s) = RWSTF w s
  liftHandle handle = RWS.Strict.RWST (\ r s -> unRWSTF <$> handle (RWSTF ((), s, mempty)) (\ (RWSTF (x, s, w)) -> toRWSTF w <$> RWS.Strict.runRWST x r s))

instance MonadTransContext (Lazy.StateT s) where
  type Context (Lazy.StateT s) = (,) s
  liftHandle handle = Lazy.StateT (\ s -> swap <$> handle (s, ()) (\ (s, x) -> swap <$> Lazy.runStateT x s))

instance MonadTransContext (Strict.StateT s) where
  type Context (Strict.StateT s) = (,) s
  liftHandle handle = Strict.StateT (\ s -> swap <$> handle (s, ()) (\ (s, x) -> swap <$> Strict.runStateT x s))

instance Monoid w => MonadTransContext (Lazy.WriterT w) where
  type Context (Lazy.WriterT w) = (,) w
  liftHandle handle = Lazy.WriterT (swap <$> handle (mempty, ()) (\ (w, x) -> swap . fmap (mappend w) <$> Lazy.runWriterT x))

instance Monoid w => MonadTransContext (Strict.WriterT w) where
  type Context (Strict.WriterT w) = (,) w
  liftHandle handle = Strict.WriterT (swap <$> handle (mempty, ()) (\ (w, x) -> swap . fmap (mappend w) <$> Strict.runWriterT x))

handling :: (Effect eff, CanThread eff (Context t), MonadTransContext t, Member eff sig, Algebra sig m, Monad (t m)) => eff (t m) a -> t m a
handling op = liftHandle (\ s dist -> handle s dist op)


-- | @m@ is a carrier for @sig@ containing @eff@.
--
-- Note that if @eff@ is a sum, it will be decomposed into multiple 'Member' constraints. While this technically allows one to combine multiple unrelated effects into a single 'Has' constraint, doing so has two significant drawbacks:
--
-- 1. Due to [a problem with recursive type families](https://gitlab.haskell.org/ghc/ghc/issues/8095), this can lead to significantly slower compiles when overused.
--
-- 2. It defeats @ghc@’s warnings for redundant constraints, and thus can lead to a proliferation of redundant constraints as code is changed.
--
-- @since 1.0.0.0
type Has eff sig m = (Members eff sig, Algebra sig m)

-- | Construct a request for an effect to be interpreted by some handler later on.
--
-- @since 0.1.0.0
send :: (Member eff sig, Algebra sig m) => eff m a -> m a
send = alg . inj
{-# INLINE send #-}


handle :: (Monad m, Effect eff, CanThread eff ctx, Member eff sig, Algebra sig n) => ctx () -> (forall x . ctx (m x) -> n (ctx x)) -> eff m a -> n (ctx a)
handle state handler = send . thread state handler
{-# INLINE handle #-}

-- | Thread a stateless handler for a carrier through an effect and eliminate it with the carrier’s algebra.
--
-- This is useful for carriers taking some input but not modifying the output. When @m@ is coercible to @n@, 'handleCoercible' may be more appropriate.
--
-- @since 1.0.0.0
handleIdentity :: (Monad m, Effect eff, Member eff sig, Algebra sig n) => (forall x . m x -> n x) -> eff m a -> n a
handleIdentity f op = liftIdentity (\ ctx dst -> handle ctx dst op) f
{-# INLINE handleIdentity #-}

-- | Thread a 'Coercible' carrier through an 'Effect'.
--
-- This is applicable whenever @m@ is 'Coercible' to @n@, e.g. simple @newtype@s.
--
-- @since 1.0.0.0
handleCoercible :: (Monad m, Effect eff, Member eff sig, Algebra sig n, Coercible m n) => eff m a -> n a
handleCoercible = handleIdentity coerce
{-# INLINE handleCoercible #-}
