module Error.Base
( tests
) where

import Error
import Pure
import Test.Tasty

tests :: TestTree
tests = testError "Either" pure genC genA genB
