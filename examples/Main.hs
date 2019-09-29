module Main
( main
) where

import qualified Inference
import qualified Parser
import qualified ReinterpretLog
import qualified Teletype
import Test.Hspec

main :: IO ()
main = hspec $ do
  Inference.spec
  Parser.spec
  ReinterpretLog.spec
  Teletype.spec
