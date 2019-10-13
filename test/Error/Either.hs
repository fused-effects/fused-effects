module Error.Either
( tests
) where

import Control.Carrier.Error.Either
import Error
import Pure
import Test.Tasty

tests :: TestTree
tests = testError "ErrorC" runError genC genA genB
