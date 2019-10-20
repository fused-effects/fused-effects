{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Fresh
( tests
, gen
, test
) where

import qualified Control.Carrier.Fresh.Strict as FreshC
import Control.Effect.Fresh
import Gen
import qualified Hedgehog.Range as R
import qualified Monad
import qualified MonadFix
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Fresh"
  [ testGroup "FreshC" $
    [ testMonad
    , testMonadFix
    , testFresh
    ] >>= ($ RunC FreshC.runFresh)
  ] where
  testMonad    run = Monad.test    (m gen) a b c (atom "(,)" (,) <*> n <*> unit) run
  testMonadFix run = MonadFix.test (m gen) a b   (atom "(,)" (,) <*> n <*> unit) run
  testFresh    run = Fresh.test n  (m gen) a                                     run
  n = Gen.integral (R.linear 0 100)


gen :: Has Fresh sig m => GenM m -> GenM m
gen _ a = atom "fmap" fmap <*> fn a <*> label "fresh" fresh


test
  :: Has Fresh sig m
  => Gen Int
  -> GenM m
  -> Gen a
  -> RunC Int ((,) Int) m
  -> [TestTree]
test n m a (RunC runFresh) =
  [ testProperty "fresh yields unique values" . forall (n :. m a :. Nil) $
    \ n m -> runFresh n (m >> fresh) /== runFresh n (m >> fresh >> fresh)
  ]
