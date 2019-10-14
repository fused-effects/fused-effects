{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Cut
( tests
, gen
, cutTests
) where

import Control.Effect.Cut
import Control.Effect.NonDet
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen
import qualified NonDet
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Cut"
  []


gen :: (Has Cut sig m, Has NonDet sig m) => (forall a . Gen a -> Gen (m a)) -> Gen a -> Gen (m a)
gen m a = choice
  [ subterm (m a) call
  , pure cutfail
  , NonDet.gen m a
  ]


cutTests :: forall a b m sig . (Has Cut sig m, Has NonDet sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a) => (forall a . m a -> PureC [a]) -> (forall a. Gen a -> Gen (Blind (m a))) -> Gen a -> Gen b -> [TestTree]
cutTests runCut m a b
  = testProperty "cutfail annihilates >>=" (forall (fn @a (m a) :. Nil)
    (\ k -> cutfail_bindAnnihilation (~=) runCut (getBlind . apply k)))
  : testProperty "cutfail annihilates <|>" (forall (m a :. Nil)
    (\ m -> cutfail_chooseAnnihilation (~=) runCut (getBlind m)))
  : testProperty "call delimits cutfail" (forall (m a :. Nil)
    (\ m -> call_delimiting (~=) runCut (getBlind m)))
  : NonDet.nonDetTests runCut m a b
