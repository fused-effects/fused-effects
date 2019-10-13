module State.StateT.Strict
( tests
) where

import qualified Control.Monad.Trans.State.Strict as StateT
import Data.Tuple (swap)
import Pure
import State.Gen
import Test.Tasty

tests :: TestTree
tests = testState "StateT (Strict)" (fmap (fmap swap) . flip StateT.runStateT) genA
