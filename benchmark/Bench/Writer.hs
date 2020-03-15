{-# LANGUAGE TypeApplications #-}
module Bench.Writer
( benchmark
) where

import Control.Carrier.Writer.Strict as C.Strict
import Data.Foldable (for_)
import Data.Monoid (Sum(..))
import Gauge hiding (benchmark)

benchmark :: Gauge.Benchmark
benchmark = bgroup "Writer"
  [ bench "Strict.WriterC" $ whnf (run . execWriter @(Sum Int) . tellLoop) n
  ]
  where
  n = 100000

tellLoop :: Has (Writer (Sum Int)) sig m => Int -> m ()
tellLoop i = for_ [1..i] (tell . Sum)
