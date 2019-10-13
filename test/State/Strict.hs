module State.Strict
( tests
) where

import Control.Carrier.State.Strict
import Pure
import State.Gen
import Test.Tasty

tests :: TestTree
tests = testState "StateC (Strict)" runState genA
