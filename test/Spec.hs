module Main where

import qualified Control.Effect.Spec
import qualified Control.Effect.IO.Spec
import qualified Control.Effect.NonDet.Spec
import Test.Hspec

main :: IO ()
main = hspec . parallel $ do
  describe "Control.Effect.Spec" Control.Effect.Spec.spec
  describe "Control.Effect.IO.Spec" Control.Effect.IO.Spec.spec
  describe "Control.Effect.NonDet.Spec" Control.Effect.NonDet.Spec.spec
