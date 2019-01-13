{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Effect.Lift
( Lift(..)
, runM
, LiftC(..)
, LiftIO(..)
, runMIO
, LiftIOC(..)
) where

import Control.Monad.IO.Class
import Control.Effect.Carrier
import Control.Effect.Internal
import Control.Effect.Lift.Internal

-- | Extract a 'Lift'ed 'Monad'ic action from an effectful computation.
runM :: Monad m => Eff (LiftC m) a -> m a
runM = runLiftC . interpret

newtype LiftC m a = LiftC { runLiftC :: m a }

instance Monad m => Carrier (Lift m) (LiftC m) where
  ret = LiftC . pure
  eff = LiftC . (>>= runLiftC) . unLift

-- | Extract a 'Lift'ed 'Monad'ic action from an effectful computation.
runMIO :: MonadIO m => Eff (LiftIOC m) a -> m a
runMIO = runLiftIOC . interpret

newtype LiftIOC m a = LiftIOC { runLiftIOC :: m a }

instance MonadIO m => Carrier (LiftIO IO) (LiftIOC m) where
  ret = LiftIOC . pure
  eff = LiftIOC . (>>= runLiftIOC) . liftIO . unLiftIO
