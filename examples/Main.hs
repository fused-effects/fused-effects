module Main
( main
) where

import qualified Inference
import qualified Parser
import qualified ReinterpretLog
import qualified Teletype
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "examples"
  [ Inference.example
  , Parser.example
  , ReinterpretLog.example
  , Teletype.example
  ]
