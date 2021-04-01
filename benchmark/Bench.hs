module Main
( main
) where

import qualified Bench.Error as Error
import qualified Bench.Interpret as Interpret
import qualified Bench.NonDet as NonDet
import qualified Bench.Reader as Reader
import qualified Bench.State as State
import qualified Bench.Writer as Writer
import           Test.Tasty.Bench

main :: IO ()
main = defaultMain
  [ Error.benchmark
  , Interpret.benchmark
  , NonDet.benchmark
  , Reader.benchmark
  , State.benchmark
  , Writer.benchmark
  ]
