module Control.Carrier.NonDet.Maybe
( -- * NonDet effects
  module Control.Effect.NonDet
  -- * NonDet carrier
, runNonDet
, NonDetC(..)
  -- * Re-exports
, Carrier
, Has
, run
) where

import Control.Carrier
import Control.Effect.NonDet

runNonDet :: NonDetC m a -> m (Maybe a)
runNonDet = runNonDetC

newtype NonDetC m a = NonDetC { runNonDetC :: m (Maybe a) }
