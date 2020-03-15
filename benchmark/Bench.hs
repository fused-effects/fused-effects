module Main
( main
) where

import qualified Bench.Error as Error
import qualified Bench.Interpret as Interpret
import qualified Bench.NonDet as NonDet
import qualified Bench.Reader as Reader
import qualified Bench.Writer as Writer
import           Gauge

main :: IO ()
main = defaultMain
  [ Error.benchmark
  , Interpret.benchmark
  , NonDet.benchmark
  , Reader.benchmark
  , Writer.benchmark
  ]
