{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module NonDet
( tests
, gen
, test
) where

import qualified Choose
import Control.Carrier
import qualified Control.Carrier.NonDet.Church as Church.NonDetC
import Control.Effect.Choose
import Control.Effect.Empty
import Control.Effect.NonDet (NonDet)
import Data.Functor.Identity (Identity(..))
import qualified Empty
import Gen
import qualified Monad
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "NonDet"
  [ testGroup "NonDetC (Church)" $
    [ testMonad
    , testNonDet
    ] >>= ($ RunL Church.NonDetC.runNonDetA)
  , testGroup "[]" $ testNonDet (RunL pure)
  ] where
  testMonad  (RunL run) = Monad.test  (m gen) a b c (pure (Identity ())) (liftRunL run)
  testNonDet run        = NonDet.test (m gen) a b                                  run


gen
  :: Has NonDet sig m
  => (forall a . Gen a -> Gen (m a))
  -> Gen a
  -> Gen (m a)
gen m a = choice [ Empty.gen m a, Choose.gen m a ]


test
  :: (Has NonDet sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a)
  => (forall a . Gen a -> Gen (m a))
  -> Gen a
  -> Gen b
  -> RunL [] m
  -> [TestTree]
test m a b (RunL runNonDet)
  =  testProperty "empty is the left identity of <|>"  (forall (m a :. Nil)
    (\ m -> runNonDet (empty <|> m) === runNonDet m))
  :  testProperty "empty is the right identity of <|>" (forall (m a :. Nil)
    (\ m -> runNonDet (m <|> empty) === runNonDet m))
  :  Empty.test  m a b (RunL runNonDet)
  ++ Choose.test m a b (RunL runNonDet)
