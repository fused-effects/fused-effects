module NonDet
( benchmark
) where

import           Gauge hiding (benchmark)
import qualified NonDet.NQueens

benchmark :: Gauge.Benchmark
benchmark = bgroup "NonDet"
  [ NonDet.NQueens.benchmark
  ]
