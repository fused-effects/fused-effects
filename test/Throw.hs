{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Throw
( genThrow
, testThrow
) where

import Control.Effect.Throw
import Hedgehog
import Hedgehog.Function
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

genThrow :: Has (Throw e) sig m => Gen e -> Gen a -> Gen (m a) -> Gen (m a)
genThrow e _ _ = throwError <$> e


testThrow :: forall e m a b sig . (Has (Throw e) sig m, Arg a, Eq b, Eq e, Show a, Show b, Show e, Vary a) => String -> (forall a . m a -> PureC (Either e a)) -> Gen e -> Gen a -> Gen b -> TestTree
testThrow name runThrow e _ b = testGroup name
  [ testProperty "throwError annihilation" . forall (e :. fn @a (Blind <$> genM [genThrow e] b) :. Nil) $
    \ e k -> throwError_annihilation (~=) runThrow e (getBlind . apply k)
  ]
