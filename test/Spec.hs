module Main
( main
) where

import qualified Control.Effect.Spec
import qualified Control.Effect.NonDet.Spec
import Test.Hspec
import Test.Tasty

main :: IO ()
main = do
  hspec . parallel $ do
    describe "Control.Effect.Spec" Control.Effect.Spec.spec

  defaultMain $ testGroup "unit tests"
    [ Control.Effect.Spec.tests
    , Control.Effect.NonDet.Spec.tests
    ]
