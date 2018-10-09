module Control.Effect.Lift
( Lift(..)
, runM
) where

import Control.Effect
import Control.Monad (join)

runM :: Monad m => Eff (Lift m) a -> m a
runM = foldH pure (join . unLift)
