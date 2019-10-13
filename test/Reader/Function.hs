module Reader.Function
( tests
) where

import Control.Carrier.Reader.Function
import Pure
import Reader
import Test.Tasty

tests :: TestTree
tests = testReader "ReaderC (Function)" runReader genA
