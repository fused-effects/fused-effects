{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Cut
( tests
, gen
, cutTests
) where

import qualified Control.Carrier.Cut.Church as CutC
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
  [ testGroup "CutC" $ cutTests CutC.runCutA
  ] where
  cutTests :: (Has Cut sig m, Has NonDet sig m) => (forall a . m a -> PureC [a]) -> [TestTree]
  cutTests run = Cut.cutTests run (genM gen) a b


gen :: (Has Cut sig m, Has NonDet sig m, Show a) => (forall a . Show a => Gen a -> Gen (With (m a))) -> Gen a -> Gen (With (m a))
gen m a = choice
  [ subterm (m a) (With "call" call <*>)
  , pure (With "cutfail" cutfail)
  , NonDet.gen m a
  ]


cutTests :: forall a b m sig . (Has Cut sig m, Has NonDet sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a) => (forall a . m a -> PureC [a]) -> (forall a . Show a => Gen a -> Gen (With (m a))) -> Gen a -> Gen b -> [TestTree]
cutTests runCut m a b
  = testProperty "cutfail annihilates >>=" (forall (fn @a (m a) :. Nil)
    (\ k -> cutfail_bindAnnihilation (~=) runCut (getWith . apply k)))
  : testProperty "cutfail annihilates <|>" (forall (m a :. Nil)
    (\ m -> cutfail_chooseAnnihilation (~=) runCut (getWith m)))
  : testProperty "call delimits cutfail" (forall (m a :. Nil)
    (\ m -> call_delimiting (~=) runCut (getWith m)))
  : NonDet.nonDetTests runCut m a b
