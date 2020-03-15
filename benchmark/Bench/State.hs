module Bench.State
( benchmark
) where

import Gauge hiding (benchmark)

benchmark :: Benchmark
benchmark = bgroup "State"
  []
