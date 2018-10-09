{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Effect.Lift
( Lift(..)
, runM
) where

import Control.Effect

runM :: Monad m => Codensity (LiftH m) a -> m a
runM = runLiftH . runCodensity var

newtype LiftH m a = LiftH { runLiftH :: m a }

instance Monad m => TermAlgebra (LiftH m) (Lift m) where
  var = LiftH . pure
  con = LiftH . (>>= runLiftH) . unLift
