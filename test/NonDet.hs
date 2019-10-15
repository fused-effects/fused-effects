{-# LANGUAGE RankNTypes #-}
module NonDet
( tests
, gen
, nonDetTests
) where

import qualified Choose
import Control.Carrier
import qualified Control.Carrier.NonDet.Church as Church.NonDetC
import qualified Control.Carrier.NonDet.Maybe as Maybe.NonDetC
import Control.Effect.Choose
import Control.Effect.Empty
import Control.Effect.NonDet (NonDet)
import qualified Control.Monad.Trans.Maybe as MaybeT
import Data.Maybe (listToMaybe, maybeToList)
import qualified Empty
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "NonDet"
  [ testGroup "NonDetC (Church)" $ nonDetTests Church.NonDetC.runNonDetA
  , testGroup "NonDetC (Maybe)"  $ nonDetTests (fmap maybeToList . Maybe.NonDetC.runNonDet)
  , testGroup "MaybeT"           $ nonDetTests (fmap maybeToList . MaybeT.runMaybeT)
  , testGroup "[]"               $ nonDetTests pure
  ] where
  nonDetTests :: Has NonDet sig m => (forall a . m a -> PureC [a]) -> [TestTree]
  nonDetTests run = NonDet.nonDetTests run (genM gen) a b


gen :: (Has NonDet sig m, Show a) => (forall a . Show a => Gen a -> Gen (With (m a))) -> Gen a -> Gen (With (m a))
gen m a = choice [ Empty.gen m a, Choose.gen m a ]


nonDetTests
  :: (Has NonDet sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a)
  => (forall a . m a -> PureC [a])
  -> (forall a . Show a => Gen a -> Gen (With (m a)))
  -> Gen a
  -> Gen b
  -> [TestTree]
nonDetTests runNonDet m a b
  =  testProperty "empty is the left identity of <|>"  (forall (m a :. Nil)
    (\ (With m) -> runNonDet (empty <|> m) === runNonDet m))
  :  testProperty "empty is the right identity of <|>" (forall (m a :. Nil)
    (\ (With m) -> runNonDet (m <|> empty) === runNonDet m))
  :  Empty.emptyTests   (fmap listToMaybe . runNonDet) m a b
  ++ Choose.chooseTests runNonDet                      m a b
