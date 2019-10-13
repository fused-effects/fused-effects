module Empty.Base
( tests
) where

import Empty
import Pure
import Test.Tasty

tests :: TestTree
tests = testEmpty "Maybe" pure genA genB
