{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, KindSignatures, RankNTypes, ScopedTypeVariables, TypeApplications, TypeOperators, UndecidableInstances #-}

-- | Provides an 'InterpretC' carrier capable of interpreting an arbitrary effect using a passed-in higher order function to interpret that effect. This is suitable for prototyping new effects quickly.

module Control.Carrier.Interpret
( -- * Interpret carrier
  runInterpret
, runInterpretState
, InterpretC(..)
, Reifies
, Handler
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import Control.Applicative (Alternative(..))
import Control.Carrier.State.Strict
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class
import Data.Functor.Const (Const(..))
import Unsafe.Coerce (unsafeCoerce)

-- | A @Handler@ is a function that interprets effects described by @sig@ into the carrier monad @m@.
newtype Handler sig m = Handler
  { runHandler :: forall s x . sig (InterpretC s sig m) x -> InterpretC s sig m x }


class Reifies s a | s -> a where
  reflect :: Const a s


data Skolem

-- | @Magic@ captures the GHC implementation detail of how single method type classes are implemented.
newtype Magic a r = Magic (Reifies Skolem a => Const r Skolem)

-- For more information on this technique, see the @reflection@ library. We use the formulation described in https://github.com/ekmett/reflection/issues/31 for better inlining.
--
-- Essentially we can view @k@ as internally a function of type @Reifies s a -> Tagged s r@, whch we can again view as just @a -> Tagged s r@ through @unsafeCoerce@. After this coercion, we just apply the function to @a@.
reify :: a -> (forall s . Reifies s a => Const r s) -> r
reify a k = unsafeCoerce (Magic k) a


-- | Interpret an effect using a higher-order function.
--
-- Note that due to the higher-rank type, you have to use either '$' or explicit application when applying this interpreter. That is, you will need to write @runInterpret f (runInterpret g myPrgram)@ or @runInterpret f $ runInterpret g $ myProgram@. If you try and write @runInterpret f . runInterpret g@, you will unfortunately get a rather scary type error!
--
-- @since 1.0.0.0
runInterpret
  :: (HFunctor eff, Monad m)
  => (forall x . eff m x -> m x)
  -> (forall s . Reifies s (Handler eff m) => InterpretC s eff m a)
  -> m a
runInterpret f m = reify (Handler (InterpretC . f . handleCoercible)) (go m) where
  go :: InterpretC s eff m x -> Const (m x) s
  go (InterpretC m) = Const m

-- | Interpret an effect using a higher-order function with some state variable.
--
-- @since 1.0.0.0
runInterpretState
  :: (HFunctor eff, Monad m)
  => (forall x . s -> eff (StateC s m) x -> m (s, x))
  -> s
  -> (forall t . Reifies t (Handler eff (StateC s m)) => InterpretC t eff (StateC s m) a)
  -> m (s, a)
runInterpretState handler state m
  = runState state
  $ runInterpret (\e -> StateC (\s -> handler s e)) m

-- | @since 1.0.0.0
newtype InterpretC s (sig :: (* -> *) -> * -> *) m a = InterpretC (m a)
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans (InterpretC s sig) where
  lift = InterpretC

instance MonadUnliftIO m => MonadUnliftIO (InterpretC s sig m) where
  askUnliftIO = InterpretC $ withUnliftIO $ \u -> return (UnliftIO (\ (InterpretC m) -> unliftIO u m))
  {-# INLINE askUnliftIO #-}
  withRunInIO inner = InterpretC $ withRunInIO $ \run -> inner (\ (InterpretC m) -> run m)
  {-# INLINE withRunInIO #-}

instance (HFunctor eff, HFunctor sig, Reifies s (Handler eff m), Monad m, Algebra sig m) => Algebra (eff :+: sig) (InterpretC s eff m) where
  eff (L eff)   = runHandler (getConst (reflect @s)) eff
  eff (R other) = InterpretC (eff (handleCoercible other))
