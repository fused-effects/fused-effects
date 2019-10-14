{-# LANGUAGE RankNTypes #-}
module Cull
( tests
, gen
, cullTests
) where

import qualified Control.Carrier.Cull.Church as CullC
import Control.Effect.Cull
import Control.Effect.NonDet (NonDet)
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen
import qualified NonDet
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Cull"
  [ testGroup "CullC" $ cullTests CullC.runCullA
  ] where
  cullTests :: (Has Cull sig m, Has NonDet sig m) => (forall a . m a -> PureC [a]) -> [TestTree]
  cullTests run = Cull.cullTests run (genM gen) a b


gen :: (Has Cull sig m, Has NonDet sig m, Show a) => (forall a . Show a => Gen a -> Gen (With (m a))) -> Gen a -> Gen (With (m a))
gen m a = choice
  [ subterm (m a) (liftWith "cull" cull)
  , NonDet.gen m a
  ]


cullTests :: (Has Cull sig m, Has NonDet sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a) => (forall a . m a -> PureC [a]) -> (forall a . Show a => Gen a -> Gen (With (m a))) -> Gen a -> Gen b -> [TestTree]
cullTests runCull m a b
  = testProperty "cull pruning" (forall (a :. m a :. m a :. Nil)
    (\ a (With m) (With n) -> cull_pruning (===) runCull a m n))
  : NonDet.nonDetTests runCull m a b
