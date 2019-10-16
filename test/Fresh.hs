{-# LANGUAGE RankNTypes #-}
module Fresh
( tests
, gen
, test
) where

import qualified Control.Carrier.Fresh.Strict as FreshC
import Control.Effect.Fresh
import Gen
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Fresh"
  [ testGroup "FreshC" $ test (m gen) a FreshC.runFresh
  ]


gen
  :: Has Fresh sig m
  => (forall a . Gen a -> Gen (m a))
  -> Gen a
  -> Gen (m a)
gen _ a = atom "fmap" fmap <*> fn a <*> label "fresh" fresh


test
  :: Has Fresh sig m
  => (forall a . Gen a -> Gen (m a))
  -> Gen a
  -> (forall a . m a -> PureC a)
  -> [TestTree]
test m a runFresh =
  [ testProperty "fresh yields unique values" . forall (m a :. Nil) $
    \ m -> runFresh (m >> fresh) /== runFresh (m >> fresh >> fresh)
  ]
