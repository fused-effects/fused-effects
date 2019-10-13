{-# LANGUAGE TypeApplications #-}
module Writer.WriterT.Strict
( tests
) where

import qualified Control.Monad.Trans.Writer.Strict as WriterT
import Data.Tuple
import Hedgehog.Gen
import Hedgehog.Range
import Pure
import Test.Tasty
import Writer.Gen

tests :: TestTree
tests = testGroup "WriterT (Strict)" (writerTests runWriter genW) where
  genW = list (linear 0 10) genA
  runWriter = fmap swap . WriterT.runWriterT
