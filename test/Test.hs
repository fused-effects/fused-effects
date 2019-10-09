module Main
( main
) where

import qualified Error.Either
import qualified Fusion
import qualified NonDet.Church
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "unit tests"
  [ Error.Either.tests
  , Fusion.tests
  , NonDet.Church.tests
  ]
