{-# LANGUAGE RankNTypes #-}
module Fresh
( tests
, gen
, test
) where

import qualified Control.Carrier.Fresh.Strict as FreshC
import Control.Effect.Fresh
import Gen
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Fresh"
  [ testGroup "FreshC" $ test (m gen) a FreshC.runFresh
  ]


gen
  :: (Has Fresh sig m, Show a)
  => (forall a . Show a => Gen a -> Gen (With (m a)))
  -> Gen a
  -> Gen (With (m a))
gen _ a = do
  f <- fn a
  pure (liftWith2 "fmap" fmap (showingFn f) (atom "fresh" fresh))


test
  :: (Has Fresh sig m, Show a)
  => (forall a . Show a => Gen a -> Gen (With (m a)))
  -> Gen a
  -> (forall a . Int -> m a -> PureC (Int, a))
  -> [TestTree]
test m a runFresh =
  [ testProperty "fresh yields unique values" . forall (G.integral (R.linear 0 100) :. m a :. Nil) $
    \ n (With m) -> runFresh n (m >> fresh) /== runFresh n (m >> fresh >> fresh)
  ]
