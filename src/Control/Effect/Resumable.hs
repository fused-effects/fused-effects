{-# LANGUAGE DeriveFunctor, DerivingStrategies, ExistentialQuantification, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Resumable
( -- * Resumable effect
  Resumable(..)
, throwResumable
  -- * Resumable carriers
, runResumableWith
, ResumableWithC(..)
  -- * Re-exports
, Carrier
, Member
, run
) where

import Control.Applicative (Alternative(..))
import Control.Carrier.Class
import Control.Carrier.Reader
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

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
