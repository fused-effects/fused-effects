module Main where

import qualified Control.Effect.Spec
import qualified Control.Effect.Reader.Spec
import Test.Hspec

main :: IO ()
main = hspec . parallel $ do
  describe "Control.Effect.Spec" Control.Effect.Spec.spec
  describe "Control.Effect.Reader.Spec" Control.Effect.Reader.Spec.spec
