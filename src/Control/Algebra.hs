{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | The 'Algebra' class is the mechanism with which effects are interpreted.

An instance of the 'Algebra' class defines an interpretation of an effect signature atop a given monad.

@since 1.0.0.0
-}
module Control.Algebra
( Algebra(..)
, Algebra'(..)
, run
, Has
, send
  -- * Re-exports
, (:+:) (..)
, module Control.Effect.Class
) where

import           Control.Effect.Catch.Internal
import           Control.Effect.Choose.Internal
import           Control.Effect.Class
import           Control.Effect.Empty.Internal
import           Control.Effect.Error.Internal
import           Control.Effect.Lift.Internal
import           Control.Effect.NonDet.Internal
import           Control.Effect.Reader.Internal
import           Control.Effect.State.Internal
import           Control.Effect.Sum ((:+:)(..), Member(..), Members)
import           Control.Effect.Throw.Internal
import           Control.Effect.Writer.Internal
import           Control.Monad ((<=<))
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Identity as Identity
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.RWS.Lazy as RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict as RWS.Strict
import qualified Control.Monad.Trans.State.Lazy as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict
import qualified Control.Monad.Trans.Writer.Lazy as Writer.Lazy
import qualified Control.Monad.Trans.Writer.Strict as Writer.Strict
import           Data.Coerce
import           Data.Functor.Identity
import           Data.List.NonEmpty (NonEmpty)
import           Data.Monoid
import qualified Data.Semigroup as S
import           Data.Tuple (swap)

-- | The class of carriers (results) for algebras (effect handlers) over signatures (effects), whose actions are given by the 'alg' method.
--
-- @since 1.0.0.0
class Monad m => Algebra sig m | m -> sig where
  -- | Construct a value in the carrier for an effect signature (typically a sum of a handled effect and any remaining effects).
  alg :: Monad n => (forall x . n x -> m x) -> sig n a -> m a

class Monad m => Algebra' sig m | m -> sig where
  alg' :: (Functor ctx, Monad n) => ctx () -> (forall x . ctx (n x) -> m (ctx x)) -> sig n a -> m (ctx a)


-- | Run an action exhausted of effects to produce its final result value.
--
-- @since 1.0.0.0
run :: Identity a -> a
run = runIdentity
{-# INLINE run #-}


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
send = alg id . inj
{-# INLINE send #-}


-- base

instance Algebra (Lift IO) IO where
  alg hom (LiftWith with k) = with (Identity ()) (coerce . hom . runIdentity) >>= hom . k . runIdentity

instance Algebra (Lift Identity) Identity where
  alg hom (LiftWith with k) = with (Identity ()) (coerce . hom . runIdentity) >>= hom . k . runIdentity

instance Algebra Choose NonEmpty where
  alg hom (Choose m) = hom (m True) S.<> hom (m False)

instance Algebra Empty Maybe where
  alg _ Empty = Nothing

instance Algebra (Error e) (Either e) where
  alg hom = \case
    L (Throw e)     -> Left e
    R (Catch m h k) -> either (hom . k <=< hom . h) (hom . k) (hom m)

instance Algebra (Reader r) ((->) r) where
  alg hom = \case
    Ask       k -> \ r -> hom (k r) r
    Local f m k -> \ r -> hom (k (hom m (f r))) r

instance Algebra NonDet [] where
  alg hom = \case
    L Empty      -> []
    R (Choose k) -> hom (k True) ++ hom (k False)

instance Monoid w => Algebra (Writer w) ((,) w) where
  alg hom = \case
    Tell w     k -> let (w', k') = hom k in (mappend w w', k')
    Listen m   k -> let (w, a) = hom m ; (w', a') = hom (k w a) in (mappend w w', a')
    Censor f m k -> let (w, a) = hom m ; (w', a') = hom (k a) in (mappend (f w) w', a')


-- transformers

instance (Algebra sig m, Effect sig) => Algebra (Error e :+: sig) (Except.ExceptT e m) where
  alg hom = \case
    L (L (Throw e))     -> Except.throwE e
    L (R (Catch m h k)) -> Except.catchE (hom m) (hom . h) >>= hom . k
    R other             -> Except.ExceptT $ alg id (thread (Right ()) (either (pure . Left) (Except.runExceptT . hom)) other)

deriving instance Algebra sig m => Algebra sig (Identity.IdentityT m)
deriving instance Algebra' sig m => Algebra' sig (Identity.IdentityT m)

#if MIN_VERSION_base(4,12,0)
-- | This instance permits effectful actions to be lifted into the 'Ap' monad
-- given a monoidal return type, which can provide clarity when chaining calls
-- to 'mappend'.
--
-- > mappend <$> act1 <*> (mappend <$> act2 <*> act3)
--
-- is equivalent to
--
-- > getAp (act1 <> act2 <> act3)
--
-- @since 1.0.1.0
deriving instance Algebra sig m => Algebra sig (Ap m)
deriving instance Algebra' sig m => Algebra' sig (Ap m)
#endif

-- | This instance permits effectful actions to be lifted into the 'Alt' monad,
-- which eases the invocation of repeated alternation with '<|>':
--
-- > a <|> b <|> c <|> d
--
-- is equivalent to
--
-- > getAlt (mconcat [a, b, c, d])
--
-- @since 1.0.1.0
deriving instance Algebra sig m => Algebra sig (Alt m)
deriving instance Algebra' sig m => Algebra' sig (Alt m)

instance Algebra sig m => Algebra (Reader r :+: sig) (Reader.ReaderT r m) where
  alg hom = \case
    L (Ask       k) -> Reader.ask >>= hom . k
    L (Local f m k) -> Reader.local f (hom m) >>= hom . k
    R other         -> Reader.ReaderT $ \ r -> alg ((`Reader.runReaderT` r) . hom) other

newtype RWSTF w s a = RWSTF { unRWSTF :: (a, s, w) }
  deriving (Functor)

toRWSTF :: Monoid w => w -> (a, s, w) -> RWSTF w s a
toRWSTF w (a, s, w') = RWSTF (a, s, mappend w w')
{-# INLINE toRWSTF #-}

instance (Algebra sig m, Effect sig, Monoid w) => Algebra (Reader r :+: Writer w :+: State s :+: sig) (RWS.Lazy.RWST r w s m) where
  alg hom = \case
    L (Ask       k)      -> RWS.Lazy.ask >>= hom . k
    L (Local f m k)      -> RWS.Lazy.local f (hom m) >>= hom . k
    R (L (Tell w k))     -> RWS.Lazy.tell w *> hom k
    R (L (Listen m k))   -> RWS.Lazy.listen (hom m) >>= hom . uncurry (flip k)
    R (L (Censor f m k)) -> RWS.Lazy.censor f (hom m) >>= hom . k
    R (R (L (Get   k)))  -> RWS.Lazy.get >>= hom . k
    R (R (L (Put s k)))  -> RWS.Lazy.put s *> hom k
    R (R (R other))      -> RWS.Lazy.RWST $ \ r s -> unRWSTF <$> alg id (thread (RWSTF ((), s, mempty)) (\ (RWSTF (x, s, w)) -> toRWSTF w <$> RWS.Lazy.runRWST (hom x) r s) other)

instance (Algebra sig m, Effect sig, Monoid w) => Algebra (Reader r :+: Writer w :+: State s :+: sig) (RWS.Strict.RWST r w s m) where
  alg hom = \case
    L (Ask       k)      -> RWS.Strict.ask >>= hom . k
    L (Local f m k)      -> RWS.Strict.local f (hom m) >>= hom . k
    R (L (Tell w k))     -> RWS.Strict.tell w *> hom k
    R (L (Listen m k))   -> RWS.Strict.listen (hom m) >>= hom . uncurry (flip k)
    R (L (Censor f m k)) -> RWS.Strict.censor f (hom m) >>= hom . k
    R (R (L (Get   k)))  -> RWS.Strict.get >>= hom . k
    R (R (L (Put s k)))  -> RWS.Strict.put s *> hom k
    R (R (R other))      -> RWS.Strict.RWST $ \ r s -> unRWSTF <$> alg id (thread (RWSTF ((), s, mempty)) (\ (RWSTF (x, s, w)) -> toRWSTF w <$> RWS.Strict.runRWST (hom x) r s) other)

instance (Algebra sig m, Effect sig) => Algebra (State s :+: sig) (State.Lazy.StateT s m) where
  alg hom = \case
    L (Get   k) -> State.Lazy.get >>= hom . k
    L (Put s k) -> State.Lazy.put s *> hom k
    R other     -> State.Lazy.StateT $ \ s -> swap <$> alg id (thread (s, ()) (\ (s, x) -> swap <$> State.Lazy.runStateT (hom x) s) other)

instance (Algebra sig m, Effect sig) => Algebra (State s :+: sig) (State.Strict.StateT s m) where
  alg hom = \case
    L (Get   k) -> State.Strict.get >>= hom . k
    L (Put s k) -> State.Strict.put s *> hom k
    R other     -> State.Strict.StateT $ \ s -> swap <$> alg id (thread (s, ()) (\ (s, x) -> swap <$> State.Strict.runStateT (hom x) s) other)

instance (Algebra sig m, Effect sig, Monoid w) => Algebra (Writer w :+: sig) (Writer.Lazy.WriterT w m) where
  alg hom = \case
    L (Tell w k)     -> Writer.Lazy.tell w *> hom k
    L (Listen m k)   -> Writer.Lazy.listen (hom m) >>= hom . uncurry (flip k)
    L (Censor f m k) -> Writer.Lazy.censor f (hom m) >>= hom . k
    R other          -> Writer.Lazy.WriterT $ swap <$> alg id (thread (mempty, ()) (\ (s, x) -> swap . fmap (mappend s) <$> Writer.Lazy.runWriterT (hom x)) other)

instance (Algebra sig m, Effect sig, Monoid w) => Algebra (Writer w :+: sig) (Writer.Strict.WriterT w m) where
  alg hom = \case
    L (Tell w k)     -> Writer.Strict.tell w *> hom k
    L (Listen m k)   -> Writer.Strict.listen (hom m) >>= hom . uncurry (flip k)
    L (Censor f m k) -> Writer.Strict.censor f (hom m) >>= hom . k
    R other          -> Writer.Strict.WriterT $ swap <$> alg id (thread (mempty, ()) (\ (s, x) -> swap . fmap (mappend s) <$> Writer.Strict.runWriterT (hom x)) other)
