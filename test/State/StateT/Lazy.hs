module State.StateT.Lazy
( tests
) where

import qualified Control.Monad.Trans.State.Lazy as StateT
import Data.Tuple (swap)
import Pure
import State.Gen
import Test.Tasty

tests :: TestTree
tests = testState "StateT (Lazy)" (fmap (fmap swap) . flip StateT.runStateT) genA
