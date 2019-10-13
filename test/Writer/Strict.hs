{-# LANGUAGE TypeApplications #-}
module Writer.Strict
( tests
) where

import Control.Carrier.Writer.Strict
import Hedgehog.Gen
import Hedgehog.Range
import Pure
import Test.Tasty
import Writer.Gen

tests :: TestTree
tests = testGroup "WriterC (Strict)" (writerTests runWriter genW) where
  genW = list (linear 0 10) genA
