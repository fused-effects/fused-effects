{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Resumable
( Resumable(..)
, throwResumable
, SomeError(..)
, runResumable
, ResumableH(..)
) where

import Control.Effect.Handler
import Control.Effect.Internal
import Control.Effect.Sum
import Data.Functor.Classes

-- | Errors which can be resumed with values of some existentially-quantified type.
data Resumable err m k
  = forall a . Resumable (err a) (a -> k)

deriving instance Functor (Resumable err m)

instance HFunctor (Resumable err) where
  hfmap _ (Resumable err k) = Resumable err k

instance Effect (Resumable err) where
  handle state handler (Resumable err k) = Resumable err (handler . (<$ state) . k)

-- | Throw an error which can be resumed with a value of its result type.
--
--   prop> run (runResumable (throwResumable (Identity a))) == Left (SomeError (Identity a))
throwResumable :: (Member (Resumable err) sig, Carrier sig m) => err a -> m a
throwResumable err = send (Resumable err gen)


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


-- | Run a 'Resumable' effect, returning uncaught errors in 'Left' and successful computations’ values in 'Right'.
--
--   prop> run (runResumable (pure a)) == Right @(SomeError Identity) @Int a
runResumable :: Effectful sig m => Eff (ResumableH err m) a -> m (Either (SomeError err) a)
runResumable = runResumableH . interpret

newtype ResumableH err m a = ResumableH { runResumableH :: m (Either (SomeError err) a) }

instance Effectful sig m => Carrier (Resumable err :+: sig) (ResumableH err m) where
  gen a = ResumableH (gen (Right a))
  alg = algE \/ (ResumableH . alg . handle (Right ()) (either (gen . Left) runResumableH))
    where algE (Resumable err _) = ResumableH (gen (Left (SomeError err)))


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> :seti -XTypeApplications
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
-- >>> import Data.Functor.Const
-- >>> import Data.Functor.Identity
