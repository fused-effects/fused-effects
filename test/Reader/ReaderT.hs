module Reader.ReaderT
( tests
) where

import qualified Control.Monad.Trans.Reader as ReaderT
import Pure
import Reader
import Test.Tasty

tests :: TestTree
tests = testReader "ReaderT" (flip ReaderT.runReaderT) genA
