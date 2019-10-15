{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

-- | A carrier for 'Lift' allowing monadic actions to be lifted into a larger context with 'sendM'.
module Control.Carrier.Lift
( -- * Lift carrier
  runM
, LiftC(..)
  -- * Lift effect
, module Control.Effect.Lift
) where

import Control.Applicative (Alternative(..))
import Control.Carrier
import Control.Effect.Lift
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class

-- | Extract a 'Lift'ed 'Monad'ic action from an effectful computation.
--
-- @since 1.0.0.0
runM :: LiftC m a -> m a
runM = runLiftC

-- | @since 1.0.0.0
newtype LiftC m a = LiftC { runLiftC :: m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans LiftC where
  lift = LiftC

instance Monad m => Carrier (Lift m) (LiftC m) where
  eff = LiftC . (>>= runLiftC) . unLift

instance MonadUnliftIO m => MonadUnliftIO (LiftC m) where
  askUnliftIO = LiftC $ withUnliftIO $ \u -> return (UnliftIO (unliftIO u . runLiftC))
  {-# INLINE askUnliftIO #-}
  withRunInIO inner = LiftC $ withRunInIO $ \run -> inner (run . runLiftC)
  {-# INLINE withRunInIO #-}
