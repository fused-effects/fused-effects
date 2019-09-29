module Main
( main
) where

import qualified Control.Effect.Spec
import qualified Control.Effect.NonDet.Spec
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "unit tests"
  [ Control.Effect.Spec.tests
  , Control.Effect.NonDet.Spec.tests
  ]
