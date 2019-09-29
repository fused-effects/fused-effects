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
    Parser.spec
    ReinterpretLog.spec
  defaultMain $ testGroup "examples"
    [ Inference.example
    , Teletype.example
    ]
