module Main
( main
) where

import qualified Inference
import qualified Parser
import qualified ReinterpretLog
import qualified Teletype
import Test.Hspec
import Test.Tasty

main :: IO ()
main = do
  hspec $ do
    Inference.spec
    Parser.spec
    ReinterpretLog.spec
    Teletype.spec
  defaultMain $ testGroup "examples"
    []
