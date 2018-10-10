{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Effect.Lift
( Lift(..)
, runM
) where

import Control.Effect.Internal
import Control.Effect.Lift.Internal

runM :: Monad m => Eff (LiftH m) a -> m a
runM = runLiftH . runEff var

newtype LiftH m a = LiftH { runLiftH :: m a }

instance Monad m => TermAlgebra (LiftH m) (Lift m) where
  var = LiftH . pure
  con = LiftH . (>>= runLiftH) . unLift
