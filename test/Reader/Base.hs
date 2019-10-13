module Reader.Base
( tests
) where

import Data.Function ((&))
import Pure
import Reader
import Test.Tasty

tests :: TestTree
tests = testReader "(->)" (fmap PureC . (&)) genA
