{-# LANGUAGE TypeApplications #-}
module Bench.State
( benchmark
) where

import Control.Carrier.State.Strict as Strict
import Data.Foldable (for_)
import Gauge hiding (benchmark)

benchmark :: Benchmark
benchmark = bgroup "State"
  [ bench "Strict.StateC" $ whnf (run . Strict.execState @Int 0 . modLoop) n
  ]
  where
  n = 100000

modLoop :: Has (State Int) sig m => Int -> m ()
modLoop i = for_ [1..i] (modify . (+))
{-# INLINE modLoop #-}
