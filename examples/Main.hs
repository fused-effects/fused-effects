module Main (main) where

import qualified Parser
import qualified Teletype
import Test.Hspec

main :: IO ()
main = hspec $ do
  Teletype.spec
  Parser.spec
