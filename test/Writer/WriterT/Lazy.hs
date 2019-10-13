{-# LANGUAGE TypeApplications #-}
module Writer.WriterT.Lazy
( tests
) where

import qualified Control.Monad.Trans.Writer.Lazy as WriterT
import Data.Tuple
import Hedgehog.Gen
import Hedgehog.Range
import Pure
import Test.Tasty
import Writer.Gen

tests :: TestTree
tests = testGroup "Writer.WriterT.Lazy" (writerTests runWriter genW) where
  genW = list (linear 0 10) genA
  runWriter = fmap swap . WriterT.runWriterT
