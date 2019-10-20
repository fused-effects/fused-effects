{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module NonDet
( tests
, gen
, test
) where

import qualified Choose
import qualified Control.Carrier.NonDet.Church as Church.NonDetC
import Control.Effect.Choose
import Control.Effect.Empty
import Control.Effect.NonDet (NonDet)
import qualified Empty
import Gen
import qualified Monad
import qualified MonadFix
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "NonDet"
  [ testGroup "NonDetC (Church)" $
    [ testMonad
    , testMonadFix
    , testNonDet
    ] >>= ($ RunL Church.NonDetC.runNonDetA)
  , testGroup "[]" $ testNonDet (RunL pure)
  ] where
  testMonad    run = Monad.test    (m gen) a b c (identity <*> unit) run
  testMonadFix run = MonadFix.test (m gen) a b   (identity <*> unit) run
  testNonDet   run = NonDet.test   (m gen) a b   (identity <*> unit) run


gen :: Has NonDet sig m => GenM m -> GenM m
gen m a = choice [ Empty.gen m a, Choose.gen m a ]


test
  :: (Has NonDet sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a)
  => GenM m
  -> Gen a
  -> Gen b
  -> Gen (Identity ())
  -> Run Identity [] m
  -> [TestTree]
test m a b i (RunL runNonDet)
  =  testProperty "empty is the left identity of <|>"  (forall (m a :. Nil)
    (\ m -> runNonDet (empty <|> m) === runNonDet m))
  :  testProperty "empty is the right identity of <|>" (forall (m a :. Nil)
    (\ m -> runNonDet (m <|> empty) === runNonDet m))
  :  Empty.test  m a b i (RunL runNonDet)
  ++ Choose.test m a b i (RunL runNonDet)
