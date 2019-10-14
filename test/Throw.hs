{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Throw
( gen
, throwTests
) where

import Control.Effect.Throw
import Hedgehog
import Hedgehog.Function
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

gen :: Has (Throw e) sig m => Gen e -> Gen (m a) -> Gen a -> Gen (m a)
gen e _ _ = throwError <$> e


throwTests :: forall e m a b sig . (Has (Throw e) sig m, Arg a, Eq b, Eq e, Show a, Show b, Show e, Vary a) => (forall a . m a -> PureC (Either e a)) -> (forall a . Gen a -> Gen (Blind (m a))) -> Gen e -> Gen a -> Gen b -> [TestTree]
throwTests runThrow m e _ b =
  [ testProperty "throwError annihilation" . forall (e :. fn @a (m b) :. Nil) $
    \ e k -> throwError_annihilation (~=) runThrow e (getBlind . apply k)
  ]
