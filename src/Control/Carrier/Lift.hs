{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

-- | A carrier for 'Lift' allowing monadic actions to be lifted into a larger context with 'sendM'.
--
-- @since 1.0.0.0
module Control.Carrier.Lift
( -- * Lift carrier
  runM
, LiftC(..)
  -- * Unlift carrier
, UnliftC(..)
  -- * Lift effect
, module Control.Effect.Lift
) where

import Control.Algebra
import Control.Applicative (Alternative)
import Control.Effect.Lift
import Control.Monad (MonadPlus, join)
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Extract a 'Lift'ed 'Monad'ic action from an effectful computation.
--
-- @since 1.0.0.0
runM :: LiftC m a -> m a
runM (LiftC m) = m

-- | @since 1.0.0.0
newtype LiftC m a = LiftC (m a)
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans LiftC where
  lift = LiftC

instance Monad m => Algebra (Lift m) (LiftC m) where
  alg = join . LiftC . unLift


newtype UnliftC m a = UnliftC { runUnlift :: m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans UnliftC where
  lift = UnliftC

instance Algebra sig m => Algebra (Unlift m :+: sig) (UnliftC m) where
  alg (L (Unlift with k)) = with runUnlift >>= k
  alg (R other)           = UnliftC (handleCoercible other)
