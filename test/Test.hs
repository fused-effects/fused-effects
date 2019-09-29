module Main
( main
) where

import qualified Fusion
import qualified NonDet
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "unit tests"
  [ Fusion.tests
  , NonDet.tests
  ]
