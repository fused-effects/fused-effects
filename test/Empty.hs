{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Empty
( genEmpty
, tests
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
  [ testEmpty "EmptyC" EmptyC.runEmpty
  , testEmpty "Maybe"  pure
  , testEmpty "MaybeT" MaybeT.runMaybeT
  ] where
  testEmpty :: Has Empty sig m => String -> (forall a . m a -> PureC (Maybe a)) -> TestTree
  testEmpty name run = Empty.testEmpty name run genA genB


genEmpty :: Has Empty sig m => Gen a -> Gen (m a) -> Gen (m a)
genEmpty _ _ = pure empty


testEmpty :: forall a b m sig . (Has Empty sig m, Arg a, Eq b, Show a, Show b, Vary a) => String -> (forall a . m a -> PureC (Maybe a)) -> Gen a -> Gen b -> TestTree
testEmpty name runEmpty _ genB = testGroup name
  [ testProperty "empty annihilation" . forall (fn @a (Blind <$> genM [genEmpty] genB) :. Nil) $
    \ k -> empty_annihilation (~=) runEmpty (getBlind . apply k)
  ]
