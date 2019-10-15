{-# LANGUAGE RankNTypes #-}
module Fresh
( tests
, gen
, freshTests
) where

import qualified Control.Carrier.Fresh.Strict as FreshC
import Control.Effect.Fresh
import Hedgehog
import Hedgehog.Function
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Fresh"
  [ testGroup "FreshC" $ freshTests FreshC.runFresh
  ] where
  freshTests :: Has Fresh sig m => (forall a . m a -> PureC a) -> [TestTree]
  freshTests run = Fresh.freshTests run (genM gen) a


gen :: (Has Fresh sig m, Show a) => (forall a . Show a => Gen a -> Gen (With (m a))) -> Gen a -> Gen (With (m a))
gen _ a = do
  f <- fn a
  pure (liftWith2 "fmap" fmap (showingFn f) (atom "fresh" fresh))


freshTests :: (Has Fresh sig m, Show a) => (forall a . m a -> PureC a) -> (forall a . Show a => Gen a -> Gen (With (m a))) -> Gen a -> [TestTree]
freshTests runFresh m a =
  [ testProperty "fresh yields unique values" . forall (m a :. Nil) $
    \ (With m) -> runFresh (m >> fresh) /== runFresh (m >> fresh >> fresh)
  ]
