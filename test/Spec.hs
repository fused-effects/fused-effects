module Main where

import qualified Control.Effect.Reader.Spec
import Test.Hspec

main :: IO ()
main = hspec . parallel $ do
  describe "Control.Effect.Reader.Spec" Control.Effect.Reader.Spec.spec
