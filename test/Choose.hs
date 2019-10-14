{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Choose
( genChoose
, testChoose
, tests
) where

import qualified Control.Carrier.Choose.Church as ChooseC
import Control.Effect.Choose
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Choose"
  [ testGroup "ChooseC" $ testChoose (ChooseC.runChooseS (pure . pure))
  ] where
  testChoose :: Has Choose sig m => (forall a . m a -> PureC [a]) -> [TestTree]
  testChoose run = Choose.testChoose run (fmap Blind . genM [genChoose]) genA genB


genChoose :: Has Choose sig m => Gen a -> Gen (m a) -> Gen (m a)
genChoose _ m = subterm2 m m (<|>)


testChoose :: forall a b m sig . (Has Choose sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a) => (forall a . m a -> PureC [a]) -> (forall a . Gen a -> Gen (Blind (m a))) -> Gen a -> Gen b -> [TestTree]
testChoose runChoose m a b =
  [ testProperty "<|> distributivity" . forall (m a :. m a :. fn @a (m b) :. Nil) $
    \ m n k -> choose_distributivity (~=) runChoose (getBlind m) (getBlind n) (getBlind . apply k)
  , testProperty "<|> associativity" . forall (m a :. m a :. m a :. Nil) $
    \ m n o -> choose_associativity (~=) runChoose (getBlind m) (getBlind n) (getBlind o)
  ]
