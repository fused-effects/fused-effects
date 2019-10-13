module Writer
( tests
) where

import qualified Writer.Strict
import qualified Writer.WriterT.Lazy
import qualified Writer.WriterT.Strict
import Test.Tasty

tests :: TestTree
tests = testGroup "Writer"
  [ Writer.Strict.tests
  , Writer.WriterT.Lazy.tests
  , Writer.WriterT.Strict.tests
  ]
