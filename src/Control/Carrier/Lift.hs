{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

-- | A carrier for 'Lift' allowing monadic actions to be lifted from an outer context into an inner one with 'sendM', and for an inner context to run actions in an outer one with 'liftWith'.
--
-- @since 1.0.0.0
module Control.Carrier.Lift
( -- * Lift carrier
  runM
, LiftC(..)
  -- * Lift effect
, module Control.Effect.Lift
) where

import Control.Algebra
import Control.Applicative (Alternative)
import Control.Effect.Lift
import Control.Monad (MonadPlus)
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Identity

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
  alg (LiftWith with k) = LiftC (with (Identity ()) (fmap Identity . runM . runIdentity)) >>= k . runIdentity
