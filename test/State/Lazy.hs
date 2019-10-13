module State.Lazy
( tests
) where

import Control.Carrier.State.Lazy
import Pure
import State.Gen
import Test.Tasty

tests :: TestTree
tests = testState "StateC (Lazy)" runState genA
