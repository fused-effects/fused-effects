{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Empty
( tests
, gen
, emptyTests
) where

import qualified Control.Carrier.Empty.Maybe as EmptyC
import Control.Effect.Empty
import qualified Control.Monad.Trans.Maybe as MaybeT
import Hedgehog
import Hedgehog.Function
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Empty"
  [ testGroup "EmptyC" $ emptyTests EmptyC.runEmpty
  , testGroup "Maybe"  $ emptyTests pure
  , testGroup "MaybeT" $ emptyTests MaybeT.runMaybeT
  ] where
  emptyTests :: Has Empty sig m => (forall a . m a -> PureC (Maybe a)) -> [TestTree]
  emptyTests run = Empty.emptyTests run (genM gen) a b


gen :: Has Empty sig m => (forall a . Gen a -> Gen (m a)) -> Gen a -> Gen (m a)
gen _ _ = pure empty


emptyTests :: forall a b m sig . (Has Empty sig m, Arg a, Eq b, Show a, Show b, Vary a) => (forall a . m a -> PureC (Maybe a)) -> (forall a. Gen a -> Gen (Blind (m a))) -> Gen a -> Gen b -> [TestTree]
emptyTests runEmpty m _ b =
  [ testProperty "empty annihilation" . forall (fn @a (m b) :. Nil) $
    \ k -> empty_annihilation (~=) runEmpty (getBlind . apply k)
  ]
