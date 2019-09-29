module Main
( main
) where

import qualified Fusion
import qualified Control.Effect.NonDet.Spec
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "unit tests"
  [ Fusion.tests
  , Control.Effect.NonDet.Spec.tests
  ]
