{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

-- | A carrier for 'Lift' allowing monadic actions to be lifted into a larger context with 'sendM'.
module Control.Carrier.Lift
( -- * Lift carrier
  runM
, LiftC(LiftC)
  -- * Lift effect
, module X
) where

import Control.Applicative (Alternative(..))
import Control.Carrier
import Control.Effect.Lift
import Control.Effect.Lift as X (Lift)
import Control.Effect.Lift as X hiding (Lift)
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class

-- | @since 1.0.0.0
newtype LiftC m a = LiftC
  { -- | Extract a 'Lift'ed 'Monad'ic action from an effectful computation.
    --
    -- @since 1.0.0.0
    runM :: m a
  }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans LiftC where
  lift = LiftC

instance Monad m => Carrier (Lift m) (LiftC m) where
  eff = LiftC . (>>= runM) . unLift

instance MonadUnliftIO m => MonadUnliftIO (LiftC m) where
  askUnliftIO = LiftC $ withUnliftIO $ \u -> return (UnliftIO (unliftIO u . runM))
  {-# INLINE askUnliftIO #-}
  withRunInIO inner = LiftC $ withRunInIO $ \run -> inner (run . runM)
  {-# INLINE withRunInIO #-}
