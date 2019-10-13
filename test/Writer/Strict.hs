module Writer.Strict
( tests
) where

import Control.Carrier.Writer.Strict
import Hedgehog.Gen
import Hedgehog.Range
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Writer

tests :: TestTree
tests = testGroup "Writer.Strict.WriterC"
  [ testProperty "tell append" . forall (genAs :. fmap Blind (Writer.gen genAs) :. Nil) $
    \ w m -> tell_append (~=) runWriter w (getBlind m)
  ] where
  genAs = list (linear 0 10) genA
