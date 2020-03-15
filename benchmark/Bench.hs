{-# LANGUAGE TypeApplications #-}
module Main
( main
) where

import           Control.Algebra
import           Control.Carrier.Writer.Strict
import           Control.Monad (replicateM_)
import           Data.Monoid (Sum(..))
import           Gauge

import qualified Bench.Error as Error
import qualified Bench.Interpret as Interpret
import qualified Bench.NonDet as NonDet
import qualified Bench.Reader as Reader

main :: IO ()
main = defaultMain
  [ Error.benchmark
  , Interpret.benchmark
  , NonDet.benchmark
  , Reader.benchmark

  , bgroup "WriterC"
    [ bench "100"   $ whnf (run . execWriter @(Sum Int) . tellLoop) 100
    , bench "1000"  $ whnf (run . execWriter @(Sum Int) . tellLoop) 1000
    , bench "10000" $ whnf (run . execWriter @(Sum Int) . tellLoop) 10000
    ]
  ]

tellLoop :: Has (Writer (Sum Int)) sig m => Int -> m ()
tellLoop i = replicateM_ i (tell (Sum (1 :: Int)))
