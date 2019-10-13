module Empty.MaybeT
( tests
) where

import qualified Control.Monad.Trans.Maybe as Maybe
import Empty
import Pure
import Test.Tasty

tests :: TestTree
tests = testEmpty "MaybeT" Maybe.runMaybeT genA genB
