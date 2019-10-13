module Empty.Maybe
( tests
) where

import Control.Carrier.Empty.Maybe
import Empty
import Pure
import Test.Tasty

tests :: TestTree
tests = testEmpty "EmptyC" runEmpty genA genB
