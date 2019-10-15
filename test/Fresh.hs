{-# LANGUAGE RankNTypes #-}
module Fresh
( tests
, gen
) where

import Control.Effect.Fresh
import Hedgehog
import Hedgehog.Function
import Pure
import Test.Tasty

tests :: TestTree
tests = testGroup "Fresh"
  []


gen :: (Has Fresh sig m, Show a) => (forall a . Show a => Gen a -> Gen (With (m a))) -> Gen a -> Gen (With (m a))
gen _ a = do
  f <- fn a
  pure (liftWith2 "fmap" fmap (showingFn f) (atom "fresh" fresh))
