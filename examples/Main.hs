module Main where

import qualified Parser
import qualified ReinterpretLog
import qualified Teletype
import Test.Hspec

main :: IO ()
main = hspec $ do
  Teletype.spec
  ReinterpretLog.spec
  Parser.spec
