{-# LANGUAGE RankNTypes #-}
module Cull
( tests
, gen
, cullTests
) where

import Control.Effect.Cull
import Control.Effect.NonDet (NonDet)
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen
import qualified NonDet
import Pure
import Test.Tasty

tests :: TestTree
tests = testGroup "Cull"
  []


gen :: (Has Cull sig m, Has NonDet sig m) => (forall a . Gen a -> Gen (m a)) -> Gen a -> Gen (m a)
gen m a = choice
  [ subterm (m a) cull
  , NonDet.gen m a
  ]


cullTests :: forall a b m sig . (Has NonDet sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a) => (forall a . m a -> PureC [a]) -> (forall a. Gen a -> Gen (Blind (m a))) -> Gen a -> Gen b -> [TestTree]
cullTests runCull m a b = NonDet.nonDetTests runCull m a b
