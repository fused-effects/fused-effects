{-# LANGUAGE DeriveFunctor, DerivingStrategies, ExistentialQuantification, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Resumable
( -- * Resumable effect
  Resumable(..)
, throwResumable
, SomeError(..)
  -- * Resumable carriers
, runResumable
, ResumableC(..)
, runResumableWith
, ResumableWithC(..)
  -- * Re-exports
, Carrier
, Member
, run
) where

import Control.Applicative (Alternative(..))
import Control.DeepSeq
import Control.Carrier.Class
import Control.Carrier.Error
import Control.Carrier.Reader
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Classes

-- | Errors which can be resumed with values of some existentially-quantified type.
data Resumable err m k
  = forall a . Resumable (err a) (a -> m k)

deriving instance Functor m => Functor (Resumable err m)

instance HFunctor (Resumable err) where
  hmap f (Resumable err k) = Resumable err (f . k)

instance Effect (Resumable err) where
  handle state handler (Resumable err k) = Resumable err (handler . (<$ state) . k)

-- | Throw an error which can be resumed with a value of its result type.
--
--   prop> run (runResumable (throwResumable (Identity a))) === Left (SomeError (Identity a))
throwResumable :: (Member (Resumable err) sig, Carrier sig m) => err a -> m a
throwResumable err = send (Resumable err pure)


-- | An error at some existentially-quantified type index.
data SomeError err
  = forall a . SomeError (err a)

-- | Equality for 'SomeError' is determined by an 'Eq1' instance for the error type.
--
--   Note that since we can’t tell whether the type indices are equal, let alone what 'Eq' instance to use for them, the comparator passed to 'liftEq' always returns 'True'. Thus, 'SomeError' is best used with type-indexed GADTs for the error type.
--
--   prop> SomeError (Identity a) === SomeError (Identity b)
--   prop> (SomeError (Const a) === SomeError (Const b)) == (a == b)
instance Eq1 err => Eq (SomeError err) where
  SomeError exc1 == SomeError exc2 = liftEq (const (const True)) exc1 exc2

-- | Ordering for 'SomeError' is determined by an 'Ord1' instance for the error type.
--
--   Note that since we can’t tell whether the type indices are equal, let alone what 'Ord' instance to use for them, the comparator passed to 'liftCompare' always returns 'EQ'. Thus, 'SomeError' is best used with type-indexed GADTs for the error type.
--
--   prop> (SomeError (Identity a) `compare` SomeError (Identity b)) === EQ
--   prop> (SomeError (Const a) `compare` SomeError (Const b)) === (a `compare` b)
instance Ord1 err => Ord (SomeError err) where
  SomeError exc1 `compare` SomeError exc2 = liftCompare (const (const EQ)) exc1 exc2

-- | Showing for 'SomeError' is determined by a 'Show1' instance for the error type.
--
--   Note that since we can’t tell what 'Show' instance to use for the type index, the functions passed to 'liftShowsPrec' always return the empty 'ShowS'. Thus, 'SomeError' is best used with type-indexed GADTs for the error type.
--
--   prop> show (SomeError (Identity a)) === "SomeError (Identity )"
--   prop> show (SomeError (Const a)) === ("SomeError (Const " ++ showsPrec 11 a ")")
instance Show1 err => Show (SomeError err) where
  showsPrec d (SomeError err) = showsUnaryWith (liftShowsPrec (const (const id)) (const id)) "SomeError" d err


-- | Evaluation of 'SomeError' to normal forms is determined by a 'NFData1' instance for the error type.
--
--   prop> pure (rnf (SomeError (Identity (error "error"))) :: SomeError Identity) `shouldThrow` errorCall "error"
instance NFData1 err => NFData (SomeError err) where
  rnf (SomeError err) = liftRnf (\a -> seq a ()) err


-- | Run a 'Resumable' effect, returning uncaught errors in 'Left' and successful computations’ values in 'Right'.
--
--   prop> run (runResumable (pure a)) === Right @(SomeError Identity) @Int a
runResumable :: ResumableC err m a -> m (Either (SomeError err) a)
runResumable = runError . runResumableC

newtype ResumableC err m a = ResumableC { runResumableC :: ErrorC (SomeError err) m a }
  deriving newtype (Alternative, Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Carrier sig m, Effect sig) => Carrier (Resumable err :+: sig) (ResumableC err m) where
  eff (L (Resumable err _)) = ResumableC (throwError (SomeError err))
  eff (R other)             = ResumableC (eff (R (handleCoercible other)))
  {-# INLINE eff #-}


-- | Run a 'Resumable' effect, resuming uncaught errors with a given handler.
--
--   Note that this may be less efficient than defining a specialized carrier type and instance specifying the handler’s behaviour directly. Performance-critical code may wish to do that to maximize the opportunities for fusion and inlining.
--
--   >>> data Err a where Err :: Int -> Err Int
--
--   prop> run (runResumableWith (\ (Err b) -> pure (1 + b)) (pure a)) === a
--   prop> run (runResumableWith (\ (Err b) -> pure (1 + b)) (throwResumable (Err a))) === 1 + a
runResumableWith :: (forall x . err x -> m x)
                 -> ResumableWithC err m a
                 -> m a
runResumableWith with = runReader (Handler with) . runResumableWithC

newtype ResumableWithC err m a = ResumableWithC { runResumableWithC :: ReaderC (Handler err m) m a }
  deriving newtype (Alternative, Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans (ResumableWithC err) where
  lift = ResumableWithC . lift
  {-# INLINE lift #-}

newtype Handler err m = Handler { runHandler :: forall x . err x -> m x }

instance Carrier sig m => Carrier (Resumable err :+: sig) (ResumableWithC err m) where
  eff (L (Resumable err k)) = ResumableWithC (ReaderC (\ handler -> runHandler handler err)) >>= k
  eff (R other)             = ResumableWithC (eff (R (handleCoercible other)))
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> :seti -XGADTs
-- >>> :seti -XTypeApplications
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
-- >>> import Data.Functor.Const
-- >>> import Data.Functor.Identity
