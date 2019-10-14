{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Choose
( genChoose
, testChoose
, tests
) where

import Control.Effect.Choose
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Choose"
  []


genChoose :: Has Choose sig m => Gen a -> Gen (m a) -> Gen (m a)
genChoose _ m = subterm2 m m (<|>)


testChoose :: forall a b m sig . (Has Choose sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a) => (forall a . m a -> PureC [a]) -> Gen a -> Gen b -> [TestTree]
testChoose runChoose a b =
  [ testProperty "<|> distributivity" . forall (op :. op :. fn @a (Blind <$> genM [genChoose] b) :. Nil) $
    \ m n k -> choose_distributivity (~=) runChoose (getBlind m) (getBlind n) (getBlind . apply k)
  , testProperty "<|> associativity" . forall (op :. op :. op :. Nil) $
    \ m n o -> choose_associativity (~=) runChoose (getBlind m) (getBlind n) (getBlind o)
  ] where
  op = Blind <$> genM [genChoose] a
