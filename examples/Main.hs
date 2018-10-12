module Main where

import qualified Teletype
import Test.Hspec

main :: IO ()
main = hspec Teletype.spec
