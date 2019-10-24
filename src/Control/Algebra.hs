{-# LANGUAGE ConstraintKinds, DeriveFunctor, EmptyCase, FlexibleInstances, FunctionalDependencies, TypeOperators, UndecidableInstances #-}

{- | The 'Algebra' class is the mechanism with which effects are interpreted.

An instance of the 'Algebra' class defines an interpretation of an effect signature atop a given monad.

@since 1.0.0.0
-}
module Control.Algebra
( Algebra(..)
, Has
, send
  -- * Re-exports
, (:+:) (..)
, run
, module Control.Effect.Class
) where

import Control.Algebra.Internal
import Control.Carrier.Pure (PureC, run)
import Control.Effect.Class
import Control.Effect.Pure
import Control.Effect.Sum ((:+:)(..), Member(..), Members)
import {-# SOURCE #-} Control.Effect.Writer (Writer(..))
import Control.Monad ((<=<), join)
import Data.Functor.Identity
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Identity as Identity
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.RWS.Lazy as RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict as RWS.Strict
import qualified Control.Monad.Trans.State.Lazy as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict
import qualified Control.Monad.Trans.Writer.Lazy as Writer.Lazy
import qualified Control.Monad.Trans.Writer.Strict as Writer.Strict
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Semigroup as S
import Data.Tuple (swap)

-- | The class of carriers (results) for algebras (effect handlers) over signatures (effects), whose actions are given by the 'alg' method.
--
-- @since 1.0.0.0
class (HFunctor sig, Monad m) => Algebra sig m | m -> sig where
  -- | Construct a value in the carrier for an effect signature (typically a sum of a handled effect and any remaining effects).
  alg :: sig m a -> m a

instance Algebra Pure PureC where
  alg v = case v of {}
  {-# INLINE alg #-}


-- | @m@ is a carrier for @sig@ containing @eff@.
--
-- Note that if @eff@ is a sum, it will be decomposed into multiple 'Member' constraints. While this technically allows one to combine multiple unrelated effects into a single 'Has' constraint, doing so has two significant drawbacks:
--
-- 1. Due to [a problem with recursive type families](https://gitlab.haskell.org/ghc/ghc/issues/8095), this can lead to significantly slower compiles.
--
-- 2. It defeats @ghc@’s warnings for redundant constraints, and thus can lead to a proliferation of redundant constraints as code is changed.
type Has eff sig m = (Members eff sig, Algebra sig m)

-- | Construct a request for an effect to be interpreted by some handler later on.
send :: (Member eff sig, Algebra sig m) => eff m a -> m a
send = alg . inj
{-# INLINE send #-}


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

instance (Algebra sig m, Effect sig) => Algebra (Error e :+: sig) (Except.ExceptT e m) where
  alg (L (L (Throw e)))     = Except.throwE e
  alg (L (R (Catch m h k))) = Except.catchE m h >>= k
  alg (R other)             = Except.ExceptT $ alg (handle (Right ()) (either (pure . Left) Except.runExceptT) other)

instance Algebra sig m => Algebra sig (Identity.IdentityT m) where
  alg = Identity.IdentityT . alg . handleCoercible

instance Algebra sig m => Algebra (Reader r :+: sig) (Reader.ReaderT r m) where
  alg (L (Ask       k)) = Reader.ask >>= k
  alg (L (Local f m k)) = Reader.local f m >>= k
  alg (R other)         = Reader.ReaderT $ \ r -> alg (hmap (flip Reader.runReaderT r) other)

newtype RWSTF w s a = RWSTF { unRWSTF :: (a, s, w) }
  deriving (Functor)

toRWSTF :: Monoid w => w -> (a, s, w) -> RWSTF w s a
toRWSTF w (a, s, w') = RWSTF (a, s, mappend w w')
{-# INLINE toRWSTF #-}

instance (Algebra sig m, Effect sig, Monoid w) => Algebra (Reader r :+: Writer w :+: State s :+: sig) (RWS.Lazy.RWST r w s m) where
  alg (L (Ask       k))      = RWS.Lazy.ask >>= k
  alg (L (Local f m k))      = RWS.Lazy.local f m >>= k
  alg (R (L (Tell w k)))     = RWS.Lazy.tell w *> k
  alg (R (L (Listen m k)))   = RWS.Lazy.listen m >>= uncurry (flip k)
  alg (R (L (Censor f m k))) = RWS.Lazy.censor f m >>= k
  alg (R (R (L (Get   k))))  = RWS.Lazy.get >>= k
  alg (R (R (L (Put s k))))  = RWS.Lazy.put s *> k
  alg (R (R (R other)))      = RWS.Lazy.RWST $ \ r s -> unRWSTF <$> alg (handle (RWSTF ((), s, mempty)) (\ (RWSTF (x, s, w)) -> toRWSTF w <$> RWS.Lazy.runRWST x r s) other)

instance (Algebra sig m, Effect sig, Monoid w) => Algebra (Reader r :+: Writer w :+: State s :+: sig) (RWS.Strict.RWST r w s m) where
  alg (L (Ask       k))      = RWS.Strict.ask >>= k
  alg (L (Local f m k))      = RWS.Strict.local f m >>= k
  alg (R (L (Tell w k)))     = RWS.Strict.tell w *> k
  alg (R (L (Listen m k)))   = RWS.Strict.listen m >>= uncurry (flip k)
  alg (R (L (Censor f m k))) = RWS.Strict.censor f m >>= k
  alg (R (R (L (Get   k))))  = RWS.Strict.get >>= k
  alg (R (R (L (Put s k))))  = RWS.Strict.put s *> k
  alg (R (R (R other)))      = RWS.Strict.RWST $ \ r s -> unRWSTF <$> alg (handle (RWSTF ((), s, mempty)) (\ (RWSTF (x, s, w)) -> toRWSTF w <$> RWS.Strict.runRWST x r s) other)

instance (Algebra sig m, Effect sig) => Algebra (State s :+: sig) (State.Lazy.StateT s m) where
  alg (L (Get   k)) = State.Lazy.get >>= k
  alg (L (Put s k)) = State.Lazy.put s *> k
  alg (R other)     = State.Lazy.StateT $ \ s -> swap <$> alg (handle (s, ()) (\ (s, x) -> swap <$> State.Lazy.runStateT x s) other)

instance (Algebra sig m, Effect sig) => Algebra (State s :+: sig) (State.Strict.StateT s m) where
  alg (L (Get   k)) = State.Strict.get >>= k
  alg (L (Put s k)) = State.Strict.put s *> k
  alg (R other)     = State.Strict.StateT $ \ s -> swap <$> alg (handle (s, ()) (\ (s, x) -> swap <$> State.Strict.runStateT x s) other)

instance (Algebra sig m, Effect sig, Monoid w) => Algebra (Writer w :+: sig) (Writer.Lazy.WriterT w m) where
  alg (L (Tell w k))     = Writer.Lazy.tell w *> k
  alg (L (Listen m k))   = Writer.Lazy.listen m >>= uncurry (flip k)
  alg (L (Censor f m k)) = Writer.Lazy.censor f m >>= k
  alg (R other)          = Writer.Lazy.WriterT $ swap <$> alg (handle (mempty, ()) (\ (s, x) -> swap . fmap (mappend s) <$> Writer.Lazy.runWriterT x) other)

instance (Algebra sig m, Effect sig, Monoid w) => Algebra (Writer w :+: sig) (Writer.Strict.WriterT w m) where
  alg (L (Tell w k))     = Writer.Strict.tell w *> k
  alg (L (Listen m k))   = Writer.Strict.listen m >>= uncurry (flip k)
  alg (L (Censor f m k)) = Writer.Strict.censor f m >>= k
  alg (R other)          = Writer.Strict.WriterT $ swap <$> alg (handle (mempty, ()) (\ (s, x) -> swap . fmap (mappend s) <$> Writer.Strict.runWriterT x) other)
