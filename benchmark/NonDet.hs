module NonDet
( benchmark
) where

import qualified Control.Carrier.NonDet.Church as NonDet.Church
import           Control.Carrier.Pure
import           Gauge hiding (benchmark)
import qualified NonDet.NQueens

benchmark :: Gauge.Benchmark
benchmark = bgroup "NonDet"
  [ NonDet.NQueens.benchmark (run . NonDet.Church.runNonDet)
  ]
