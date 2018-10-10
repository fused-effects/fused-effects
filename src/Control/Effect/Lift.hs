{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Effect.Lift
( Lift(..)
, runM
) where

import Control.Effect.Handler
import Control.Effect.Internal
import Control.Effect.Lift.Internal

runM :: Monad m => Eff (LiftH m) a -> m a
runM = runLiftH . interpret

newtype LiftH m a = LiftH { runLiftH :: m a }

instance Monad m => Carrier (Lift m) (LiftH m) where
  gen = LiftH . pure
  alg = LiftH . (>>= runLiftH) . unLift
