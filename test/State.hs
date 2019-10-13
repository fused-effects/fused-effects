module State
( tests
) where

import qualified State.Lazy
import qualified State.StateT.Lazy
import qualified State.StateT.Strict
import qualified State.Strict
import Test.Tasty

tests :: TestTree
tests = testGroup "State"
  [ State.Lazy.tests
  , State.StateT.Lazy.tests
  , State.StateT.Strict.tests
  , State.Strict.tests
  ]
