{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Effect.Lift
( Lift(..)
, runM
, LiftC(..)
) where

import Control.Effect.Carrier
import Control.Effect.Internal
import Control.Effect.Lift.Internal

-- | Extract a 'Lift'ed 'Monad'ic action from an effectful computation.
runM :: Monad m => Eff (LiftC m) a -> m a
runM = runLiftC . interpret

newtype LiftC m a = LiftC { runLiftC :: m a }

instance Monad m => Carrier (Lift m) (LiftC m) where
  handleReturn = LiftC . pure
  handleEffect = LiftC . (>>= runLiftC) . unLift
