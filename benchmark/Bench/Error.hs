{-# LANGUAGE TypeApplications #-}
module Bench.Error
( benchmark
) where

import Control.Carrier.Error.Church as Church
import Control.Carrier.Error.Either as Either
import Control.Monad (replicateM_)
import Gauge hiding (benchmark)

benchmark :: Gauge.Benchmark
benchmark = bgroup "Error"
  [ bench "Either"        $ whnf (errorLoop :: Int -> Either Int ()) 10000
  , bench "Either.ErrorC" $ whnf (run . Either.runError @Int . errorLoop) 10000
  , bench "Church.ErrorC" $ whnf (run . Church.runError @Int (pure . Left) (pure . Right) . errorLoop) 10000
  ]

errorLoop :: Has (Error Int) sig m => Int -> m ()
errorLoop i = replicateM_ i (throwError i `catchError` pure @_ @Int)
{-# INLINE errorLoop #-}
