{-# LANGUAGE RankNTypes #-}
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
import Data.Maybe (listToMaybe)
import qualified Empty
import Gen
import qualified Monad
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "NonDet"
  [ test "NonDetC (Church)" Church.NonDetC.runNonDetA
  , test "[]"               pure
  ] where
  test :: Has NonDet sig m => String -> (forall a . m a -> PureC [a]) -> TestTree
  test name run = testGroup name
    $  Monad.test  (m gen) a b c (pure (Identity ())) (run . runIdentity)
    ++ NonDet.test (m gen) a b                        run


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
  -> (forall a . m a -> PureC [a])
  -> [TestTree]
test m a b runNonDet
  =  testProperty "empty is the left identity of <|>"  (forall (m a :. Nil)
    (\ m -> runNonDet (empty <|> m) === runNonDet m))
  :  testProperty "empty is the right identity of <|>" (forall (m a :. Nil)
    (\ m -> runNonDet (m <|> empty) === runNonDet m))
  :  Empty.test  m a b (fmap listToMaybe . runNonDet)
  ++ Choose.test m a b runNonDet
