{-# LANGUAGE TypeApplications #-}
module Bench.Writer
( benchmark
) where

import Control.Carrier.Writer.Strict as C.Strict
import Control.Monad (replicateM_)
import Data.Monoid (Sum(..))
import Gauge hiding (benchmark)

benchmark :: Gauge.Benchmark
benchmark = bgroup "Writer"
  [ bench "Strict.WriterC" $ whnf (run . execWriter @(Sum Int) . tellLoop) n
  ]
  where
  n = 100000

tellLoop :: Has (Writer (Sum Int)) sig m => Int -> m ()
tellLoop i = replicateM_ i (tell (Sum (1 :: Int)))
