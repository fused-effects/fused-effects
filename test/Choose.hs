{-# LANGUAGE RankNTypes #-}
module Choose
( tests
, gen
, chooseTests
) where

import qualified Control.Carrier.Choose.Church as ChooseC
import Control.Effect.Choose
import Data.List.NonEmpty
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Choose"
  [ testGroup "ChooseC"  $ chooseTests (ChooseC.runChooseS (pure . pure))
  , testGroup "NonEmpty" $ chooseTests (pure . toList)
  ] where
  chooseTests :: Has Choose sig m => (forall a . m a -> PureC [a]) -> [TestTree]
  chooseTests run = Choose.chooseTests run (genM gen) a b


gen :: (Has Choose sig m, Show a) => (forall a . Show a => Gen a -> Gen (With (m a))) -> Gen a -> Gen (With (m a))
gen m a = subterm2 (m a) (m a) (\ l r -> With "(<|>)" (<|>) <*> l <*> r)


chooseTests :: (Has Choose sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a) => (forall a . m a -> PureC [a]) -> (forall a . Show a => Gen a -> Gen (With (m a))) -> Gen a -> Gen b -> [TestTree]
chooseTests runChoose m a b =
  [ testProperty "<|> distributivity" . forall (m a :. m a :. fn (m b) :. Nil) $
    \ m n k -> choose_distributivity (===) runChoose (getWith m) (getWith n) (getWith . apply k)
  , testProperty "<|> associativity" . forall (m a :. m a :. m a :. Nil) $
    \ m n o -> choose_associativity (===) runChoose (getWith m) (getWith n) (getWith o)
  ]
