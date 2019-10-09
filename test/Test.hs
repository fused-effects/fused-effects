module Main
( main
) where

import qualified Empty.Maybe
import qualified Error.Either
import qualified Fusion
import qualified NonDet.Church
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "unit tests"
  [ Empty.Maybe.tests
  , Error.Either.tests
  , Fusion.tests
  , NonDet.Church.tests
  ]
