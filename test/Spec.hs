module Main where

import qualified Control.Effect.Spec
import qualified Control.Effect.LiftIO
import qualified Control.Effect.NonDet.Spec
import Test.Hspec

main :: IO ()
main = hspec . parallel $ do
  describe "Control.Effect.Spec" Control.Effect.Spec.spec
  describe "Control.Effect.NonDet.Spec" Control.Effect.NonDet.Spec.spec
  describe "Control.Effect.LiftIO" Control.Effect.LiftIO.spec
