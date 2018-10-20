{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, RankNTypes, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Resumable
( Resumable(..)
, throwResumable
, SomeError(..)
, runResumable
, ResumableC(..)
, runResumableWith
, ResumableWithC(..)
) where

import Control.DeepSeq
import Control.Effect.Carrier
import Control.Effect.Internal
import Control.Effect.Sum
import Data.Functor.Classes

-- | Errors which can be resumed with values of some existentially-quantified type.
data Resumable err m k
  = forall a . Resumable (err a) (a -> k)

deriving instance Functor (Resumable err m)

instance HFunctor (Resumable err) where
  hmap _ (Resumable err k) = Resumable err k

instance Effect (Resumable err) where
  handle state handler (Resumable err k) = Resumable err (handler . (<$ state) . k)

-- | Throw an error which can be resumed with a value of its result type.
--
--   prop> run (runResumable (throwResumable (Identity a))) == Left (SomeError (Identity a))
throwResumable :: (Member (Resumable err) sig, Carrier sig m) => err a -> m a
throwResumable err = send (Resumable err ret)


-- | An error at some existentially-quantified type index.
data SomeError (err :: * -> *)
  = forall a . SomeError (err a)

-- | Equality for 'SomeError' is determined by an 'Eq1' instance for the error type.
--
--   Note that since we can’t tell whether the type indices are equal, let alone what 'Eq' instance to use for them, the comparator passed to 'liftEq' always returns 'True'. Thus, 'SomeError' is best used with type-indexed GADTs for the error type.
--
--   prop> SomeError (Identity a) == SomeError (Identity b)
--   prop> (SomeError (Const a) == SomeError (Const b)) == (a == b)
instance Eq1 err => Eq (SomeError err) where
  SomeError exc1 == SomeError exc2 = liftEq (const (const True)) exc1 exc2

-- | Ordering for 'SomeError' is determined by an 'Ord1' instance for the error type.
--
--   Note that since we can’t tell whether the type indices are equal, let alone what 'Ord' instance to use for them, the comparator passed to 'liftCompare' always returns 'EQ'. Thus, 'SomeError' is best used with type-indexed GADTs for the error type.
--
--   prop> (SomeError (Identity a) `compare` SomeError (Identity b)) == EQ
--   prop> (SomeError (Const a) `compare` SomeError (Const b)) == (a `compare` b)
instance Ord1 err => Ord (SomeError err) where
  SomeError exc1 `compare` SomeError exc2 = liftCompare (const (const EQ)) exc1 exc2

-- | Showing for 'SomeError' is determined by a 'Show1' instance for the error type.
--
--   Note that since we can’t tell what 'Show' instance to use for the type index, the functions passed to 'liftShowsPrec' always return the empty 'ShowS'. Thus, 'SomeError' is best used with type-indexed GADTs for the error type.
--
--   prop> show (SomeError (Identity a)) == "SomeError (Identity )"
--   prop> show (SomeError (Const a)) == ("SomeError (Const " ++ showsPrec 11 a ")")
instance (Show1 err) => Show (SomeError err) where
  showsPrec d (SomeError err) = showsUnaryWith (liftShowsPrec (const (const id)) (const id)) "SomeError" d err


-- | Evaluation of 'SomeError' to normal forms is determined by a 'NFData1' instance for the error type.
--
--   prop> pure (rnf (SomeError (Identity (error "error"))) :: SomeError Identity) `shouldThrow` errorCall "error"
instance NFData1 err => NFData (SomeError err) where
  rnf (SomeError err) = liftRnf (\a -> seq a ()) err


-- | Run a 'Resumable' effect, returning uncaught errors in 'Left' and successful computations’ values in 'Right'.
--
--   prop> run (runResumable (pure a)) == Right @(SomeError Identity) @Int a
runResumable :: (Carrier sig m, Effect sig) => Eff (ResumableC err m) a -> m (Either (SomeError err) a)
runResumable = runResumableC . interpret

newtype ResumableC err m a = ResumableC { runResumableC :: m (Either (SomeError err) a) }

instance (Carrier sig m, Effect sig) => Carrier (Resumable err :+: sig) (ResumableC err m) where
  ret a = ResumableC (ret (Right a))
  eff = ResumableC . (alg \/ eff . handle (Right ()) (either (ret . Left) runResumableC))
    where alg (Resumable err _) = ret (Left (SomeError err))


-- | Run a 'Resumable' effect, resuming uncaught errors with a given handler.
--
--   Note that this may be less efficient than defining a specialized carrier type and instance specifying the handler’s behaviour directly. Performance-critical code may wish to do that to maximize the opportunities for fusion and inlining.
--
--   >>> data Err a where Err :: Int -> Err Int
--
--   prop> run (runResumableWith (\ (Err b) -> pure (1 + b)) (pure a)) == a
--   prop> run (runResumableWith (\ (Err b) -> pure (1 + b)) (throwResumable (Err a))) == 1 + a
runResumableWith :: (Carrier sig m, Monad m)
                 => (forall x . err x -> m x)
                 -> Eff (ResumableWithC err m) a
                 -> m a
runResumableWith with = runResumableWithC with . interpret

newtype ResumableWithC err m a = ResumableWithC ((forall x . err x -> m x) -> m a)

runResumableWithC :: (forall x . err x -> m x) -> ResumableWithC err m a -> m a
runResumableWithC f (ResumableWithC m) = m f

instance (Carrier sig m, Monad m) => Carrier (Resumable err :+: sig) (ResumableWithC err m) where
  ret a = ResumableWithC (const (ret a))
  eff op = ResumableWithC (\ handler -> (alg handler \/ eff . handlePure (runResumableWithC handler)) op)
    where alg :: Monad m => (forall x . err x -> m x) -> Resumable err (ResumableWithC err m) (ResumableWithC err m a) -> m a
          alg handler (Resumable err k) = handler err >>= runResumableWithC handler . k


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> :seti -XGADTs
-- >>> :seti -XTypeApplications
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
-- >>> import Data.Functor.Const
-- >>> import Data.Functor.Identity
