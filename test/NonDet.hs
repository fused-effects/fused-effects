{-# LANGUAGE RankNTypes #-}
module NonDet
( tests
, genNonDet
, nonDetTests
) where

import qualified Choose
import Control.Carrier
import Control.Effect.Choose
import Control.Effect.NonDet (NonDet)
import Data.Maybe (listToMaybe)
import qualified Empty
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "NonDet"
  []


genNonDet :: Has NonDet sig m => Gen a -> Gen (m a) -> Gen (m a)
genNonDet a m = choice [ Empty.gen a m, Choose.gen a m ]


nonDetTests :: forall a b m sig . (Has NonDet sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a) => (forall a . m a -> PureC [a]) -> (forall a. Gen a -> Gen (Blind (m a))) -> Gen a -> Gen b -> [TestTree]
nonDetTests runNonDet m a b
  =  testProperty "<|> left identity" (forall (m a :. Nil)
    (\ m -> choose_leftIdentity (~=) runNonDet (getBlind m)))
  :  testProperty "<|> right identity" (forall (m a :. Nil)
    (\ m -> choose_rightIdentity (~=) runNonDet (getBlind m)))
  :  Empty.emptyTests   (fmap listToMaybe . runNonDet) m a b
  ++ Choose.chooseTests runNonDet m a b
