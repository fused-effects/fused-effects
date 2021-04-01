{-# LANGUAGE TypeApplications #-}
module Bench.NonDet
( benchmark
) where

import qualified Bench.NonDet.NQueens as NQueens
import           Control.Algebra
import qualified Control.Carrier.NonDet.Church as NonDet.Church
import           Test.Tasty.Bench hiding (benchmark)

benchmark :: Benchmark
benchmark = bgroup "NonDet"
  [ bgroup "N-queens problem"
    [ NQueens.benchmark "NonDet.Church" (run . NonDet.Church.runNonDetA)
    , NQueens.benchmark "[]"            (id @[_])
    ]
  ]
