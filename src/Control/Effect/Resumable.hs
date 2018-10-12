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


data SomeError (err :: * -> *)
  = forall a . SomeError (err a)

instance Eq1 err => Eq (SomeError err) where
  SomeError exc1 == SomeError exc2 = liftEq (const (const True)) exc1 exc2

instance Ord1 err => Ord (SomeError err) where
  SomeError exc1 `compare` SomeError exc2 = liftCompare (const (const EQ)) exc1 exc2

instance (Show1 err) => Show (SomeError err) where
  showsPrec num (SomeError err) = liftShowsPrec (const (const id)) (const id) num err


runResumable :: Effectful sig m => Eff (ResumableH err m) a -> m (Either (SomeError err) a)
runResumable = runResumableH . interpret

newtype ResumableH err m a = ResumableH { runResumableH :: m (Either (SomeError err) a) }

instance Effectful sig m => Carrier (Resumable err :+: sig) (ResumableH err m) where
  gen a = ResumableH (gen (Right a))
  alg = algE \/ (ResumableH . alg . handle (Right ()) (either (gen . Left) runResumableH))
    where algE (Resumable err _) = ResumableH (gen (Left (SomeError err)))


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
-- >>> import Data.Functor.Identity
