module Main
( main
) where

import           Hedgehog.Main
import qualified Inference
import qualified Labelled
import qualified Parser
import qualified ReinterpretLog
import qualified Teletype
import           Utils

main :: IO ()
main = defaultMain $ map checkTestTree
  [ Inference.example
  , Parser.example
  , ReinterpretLog.example
  , Teletype.example
  , Labelled.example
  ]
