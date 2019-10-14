module NonDet
( tests
, genNonDet
) where

import qualified Choose
import Control.Carrier
import Control.Effect.NonDet (NonDet)
import qualified Empty
import Hedgehog
import Hedgehog.Gen
import Test.Tasty

tests :: TestTree
tests = testGroup "NonDet"
  []


genNonDet :: Has NonDet sig m => Gen a -> Gen (m a) -> Gen (m a)
genNonDet a m = choice [ Empty.genEmpty a m, Choose.gen a m ]
