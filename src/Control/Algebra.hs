{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | The 'Algebra' class is the mechanism with which effects are interpreted.

An instance of the 'Algebra' class defines an interpretation of an effect signature atop a given monad.

@since 1.0.0.0
-}
module Control.Algebra
( Handler
, Algebra(..)
, thread
, run
, Has
, send
  -- * Re-exports
, (:+:) (..)
) where

import           Control.Algebra.Internal
import           Control.Effect.Catch.Internal
import           Control.Effect.Choose.Internal
import           Control.Effect.Empty.Internal
import           Control.Effect.Error.Internal
import           Control.Effect.Lift.Internal
import           Control.Effect.NonDet.Internal
import           Control.Effect.Reader.Internal
import           Control.Effect.State.Internal
import           Control.Effect.Sum ((:+:)(..), Member(..), Members)
import           Control.Effect.Throw.Internal
import           Control.Effect.Writer.Internal
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Identity as Identity
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.RWS.Lazy as RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict as RWS.Strict
import qualified Control.Monad.Trans.State.Lazy as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict
import qualified Control.Monad.Trans.Writer.Lazy as Writer.Lazy
import qualified Control.Monad.Trans.Writer.Strict as Writer.Strict
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.List.NonEmpty (NonEmpty)
import           Data.Monoid
import qualified Data.Semigroup as S
import           Data.Tuple (swap)

-- | The class of carriers (results) for algebras (effect handlers) over signatures (effects), whose actions are given by the 'alg' method.
--
-- @since 1.0.0.0
class Monad m => Algebra sig m | m -> sig where
  -- | Interpret an effect, running any nested actions using a 'Handler' starting from an initial state in @ctx@.
  --
  -- Instances receive a signature of effects containing actions in @n@ which can be lowered to @m@ using the passed 'Handler' and initial context. Continuations in @n@ can be handled after mapping into contexts returned from previous actions.
  --
  -- For example, considering the 'Algebra' instance for @'Either' e@:
  --
  -- > instance Algebra (Error e) (Either e) where
  -- >   alg hdl sig ctx = case sig of
  -- >     L (Throw e)     -> Left e
  -- >     R (Catch m h k) -> either (hdl . (<$ ctx) . h) pure (hdl (m <$ ctx)) >>= hdl . fmap k
  --
  -- The 'Catch' case holds actions @m :: n x@ and @h :: e -> n x@ (for some existentially-quantified type @x@), and a continuation @k :: x -> n a@. The algebra must return @m (ctx a)@, so we have to ultimately use and lower the continuation in order to produce that type. The continuation takes an @x@, which we can get from either of the actions, after lowering them to values in @'Either' e@.
  --
  -- To that end, the algebra lifts both the action @m@ and the result of the error handler @h@ into the initial context @ctx@ before lowering them with @hdl@. The continuation @k@ is 'fmap'ed into the resulting context and then itself lowered with @hdl@.
  --
  -- By contrast, the 'Throw' case can simply return a value in 'Left', since there is no continuation to call—it represents an exceptional return—and @'Left' e :: forall a . Either e a@ (i.e. 'Left' is polymorphic in @a@).
  --
  -- Instances for monad transformers will most likely handle a signature containing multiple effects, with the tail of the signature handled by whatever monad the transformer wraps. In these cases, the tail of the signature can be delegated most conveniently using 'thread'; see the 'Algebra' instances for @transformers@ types such as 'Reader.ReaderT' and 'Except.ExceptT' for details.
  alg
    :: Functor ctx
    => Handler ctx n m -- ^ A 'Handler' lowering computations inside the effect into the carrier type @m@.
    -> sig n a         -- ^ The effect signature to be interpreted.
    -> ctx ()          -- ^ The initial state.
    -> m (ctx a)       -- ^ The interpretation of the effect in @m@.

-- | Compose and thread a pair of handlers and input state through the algebra for some underlying signature.
--
-- @since 1.1.0.0
thread
  :: ( Functor ctx1
     , Functor ctx2
     , Algebra sig m
     )
  => Handler ctx1 n m
  -> Handler ctx2 o n
  -> sig o a
  -> ctx1 (ctx2 ())
  -> m (ctx1 (ctx2 a))
thread hdl1 hdl2 sig = fmap getCompose . alg (fmap Compose . hdl1 . fmap hdl2 . getCompose) sig . Compose
{-# INLINE thread #-}


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
--
-- @since 1.0.0.0
type Has eff sig m = (Members eff sig, Algebra sig m)

-- | Construct a request for an effect to be interpreted by some handler later on.
--
-- @since 0.1.0.0
send :: (Member eff sig, Algebra sig m) => eff m a -> m a
send sig = runIdentity <$> alg (fmap Identity . runIdentity) (inj sig) (Identity ())
{-# INLINE send #-}


-- base

instance Algebra (Lift IO) IO where
  alg hdl (LiftWith with k) ctx = with hdl ctx >>= hdl . fmap k

instance Algebra (Lift Identity) Identity where
  alg hdl (LiftWith with k) ctx = with hdl ctx >>= hdl . fmap k

instance Algebra Choose NonEmpty where
  alg hdl (Choose m) ctx = hdl (m True <$ ctx) S.<> hdl (m False <$ ctx)

instance Algebra Empty Maybe where
  alg _ Empty _ = Nothing

instance Algebra (Error e) (Either e) where
  alg hdl sig ctx = case sig of
    L (Throw e)     -> Left e
    R (Catch m h k) -> either (hdl . (<$ ctx) . h) pure (hdl (m <$ ctx)) >>= hdl . fmap k

instance Algebra (Reader r) ((->) r) where
  alg hdl sig ctx = case sig of
    Ask       k -> \ r -> hdl (k r <$ ctx) r
    Local f m k -> \ r -> hdl (fmap k (hdl (m <$ ctx) (f r))) r

instance Algebra NonDet [] where
  alg hdl sig ctx = case sig of
    L Empty      -> []
    R (Choose k) -> hdl (k True <$ ctx) ++ hdl (k False <$ ctx)

instance Monoid w => Algebra (Writer w) ((,) w) where
  alg hdl sig ctx = case sig of
    Tell w     k -> let (w', k') = hdl (k <$ ctx) in (mappend w w', k')
    Listen m   k -> let (w, a) = hdl (m <$ ctx) ; (w', a') = hdl (fmap (k w) a) in (mappend w w', a')
    Censor f m k -> let (w, a) = hdl (m <$ ctx) ; (w', a') = hdl (fmap k a) in (mappend (f w) w', a')


-- transformers

instance Algebra sig m => Algebra (Error e :+: sig) (Except.ExceptT e m) where
  alg hdl sig ctx = case sig of
    L (L (Throw e))     -> Except.throwE e
    L (R (Catch m h k)) -> Except.catchE (hdl (m <$ ctx)) (hdl . (<$ ctx) . h) >>= hdl . fmap k
    R other             -> Except.ExceptT $ thread (either (pure . Left) Except.runExceptT) hdl other (Right ctx)

deriving instance Algebra sig m => Algebra sig (Identity.IdentityT m)

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
#endif

-- | This instance permits effectful actions to be lifted into the 'Alt' monad,
-- which eases the invocation of repeated alternation with 'Control.Applicative.<|>':
--
-- > a <|> b <|> c <|> d
--
-- is equivalent to
--
-- > getAlt (mconcat [a, b, c, d])
--
-- @since 1.0.1.0
deriving instance Algebra sig m => Algebra sig (Alt m)

instance Algebra sig m => Algebra (Reader r :+: sig) (Reader.ReaderT r m) where
  alg hdl sig ctx = case sig of
    L (Ask       k) -> Reader.ask >>= hdl . (<$ ctx) . k
    L (Local f m k) -> Reader.local f (hdl (m <$ ctx)) >>= hdl . fmap k
    R other         -> Reader.ReaderT $ \ r -> alg ((`Reader.runReaderT` r) . hdl) other ctx

newtype RWSTF w s a = RWSTF { unRWSTF :: (a, s, w) }
  deriving (Functor)

toRWSTF :: Monoid w => w -> (a, s, w) -> RWSTF w s a
toRWSTF w (a, s, w') = RWSTF (a, s, mappend w w')
{-# INLINE toRWSTF #-}

instance (Algebra sig m, Monoid w) => Algebra (Reader r :+: Writer w :+: State s :+: sig) (RWS.Lazy.RWST r w s m) where
  alg hdl sig ctx = case sig of
    L (Ask       k)      -> RWS.Lazy.ask >>= hdl . (<$ ctx) . k
    L (Local f m k)      -> RWS.Lazy.local f (hdl (m <$ ctx)) >>= hdl . fmap k
    R (L (Tell w k))     -> RWS.Lazy.tell w *> hdl (k <$ ctx)
    R (L (Listen m k))   -> RWS.Lazy.listen (hdl (m <$ ctx)) >>= hdl . uncurry (fmap . k) . swap
    R (L (Censor f m k)) -> RWS.Lazy.censor f (hdl (m <$ ctx)) >>= hdl . fmap k
    R (R (L (Get   k)))  -> RWS.Lazy.get >>= hdl . (<$ ctx) . k
    R (R (L (Put s k)))  -> RWS.Lazy.put s *> hdl (k <$ ctx)
    R (R (R other))      -> RWS.Lazy.RWST $ \ r s -> unRWSTF <$> thread (\ (RWSTF (x, s, w)) -> toRWSTF w <$> RWS.Lazy.runRWST x r s) hdl other (RWSTF (ctx, s, mempty))

instance (Algebra sig m, Monoid w) => Algebra (Reader r :+: Writer w :+: State s :+: sig) (RWS.Strict.RWST r w s m) where
  alg hdl sig ctx = case sig of
    L (Ask       k)      -> RWS.Strict.ask >>= hdl . (<$ ctx) . k
    L (Local f m k)      -> RWS.Strict.local f (hdl (m <$ ctx)) >>= hdl . fmap k
    R (L (Tell w k))     -> RWS.Strict.tell w *> hdl (k <$ ctx)
    R (L (Listen m k))   -> RWS.Strict.listen (hdl (m <$ ctx)) >>= hdl . uncurry (fmap . k) . swap
    R (L (Censor f m k)) -> RWS.Strict.censor f (hdl (m <$ ctx)) >>= hdl . fmap k
    R (R (L (Get   k)))  -> RWS.Strict.get >>= hdl . (<$ ctx) . k
    R (R (L (Put s k)))  -> RWS.Strict.put s *> hdl (k <$ ctx)
    R (R (R other))      -> RWS.Strict.RWST $ \ r s -> unRWSTF <$> thread (\ (RWSTF (x, s, w)) -> toRWSTF w <$> RWS.Strict.runRWST x r s) hdl other (RWSTF (ctx, s, mempty))

instance Algebra sig m => Algebra (State s :+: sig) (State.Lazy.StateT s m) where
  alg hdl sig ctx = case sig of
    L (Get   k) -> State.Lazy.get >>= hdl . (<$ ctx) . k
    L (Put s k) -> State.Lazy.put s *> hdl (k <$ ctx)
    R other     -> State.Lazy.StateT $ \ s -> swap <$> thread (\ (s, x) -> swap <$> State.Lazy.runStateT x s) hdl other (s, ctx)

instance Algebra sig m => Algebra (State s :+: sig) (State.Strict.StateT s m) where
  alg hdl sig ctx = case sig of
    L (Get   k) -> State.Strict.get >>= hdl . (<$ ctx) . k
    L (Put s k) -> State.Strict.put s *> hdl (k <$ ctx)
    R other     -> State.Strict.StateT $ \ s -> swap <$> thread (\ (s, x) -> swap <$> State.Strict.runStateT x s) hdl other (s, ctx)

instance (Algebra sig m, Monoid w) => Algebra (Writer w :+: sig) (Writer.Lazy.WriterT w m) where
  alg hdl sig ctx = case sig of
    L (Tell w k)     -> Writer.Lazy.tell w *> hdl (k <$ ctx)
    L (Listen m k)   -> Writer.Lazy.listen (hdl (m <$ ctx)) >>= hdl . uncurry (fmap . k) . swap
    L (Censor f m k) -> Writer.Lazy.censor f (hdl (m <$ ctx)) >>= hdl . fmap k
    R other          -> Writer.Lazy.WriterT $ swap <$> thread (\ (s, x) -> swap . fmap (mappend s) <$> Writer.Lazy.runWriterT x) hdl other (mempty, ctx)

instance (Algebra sig m, Monoid w) => Algebra (Writer w :+: sig) (Writer.Strict.WriterT w m) where
  alg hdl sig ctx = case sig of
    L (Tell w k)     -> Writer.Strict.tell w *> hdl (k <$ ctx)
    L (Listen m k)   -> Writer.Strict.listen (hdl (m <$ ctx)) >>= hdl . uncurry (fmap . k) . swap
    L (Censor f m k) -> Writer.Strict.censor f (hdl (m <$ ctx)) >>= hdl . fmap k
    R other          -> Writer.Strict.WriterT $ swap <$> thread (\ (s, x) -> swap . fmap (mappend s) <$> Writer.Strict.runWriterT x) hdl other (mempty, ctx)
