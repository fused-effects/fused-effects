module Error.ExceptT
( tests
) where

import qualified Control.Monad.Trans.Except as Except
import Error
import Pure
import Test.Tasty

tests :: TestTree
tests = testError "ExceptT" Except.runExceptT genC genA genB
