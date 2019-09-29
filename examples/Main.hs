module Main
( main
) where

import qualified Parser
import qualified ReinterpretLog
import qualified Teletype
import Test.Hspec

main :: IO ()
main = hspec $ do
  Parser.spec
  ReinterpretLog.spec
  Teletype.spec
