{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Throw
( tests
, gen
, throwTests
) where

import qualified Control.Carrier.Throw.Either as ThrowC
import Control.Effect.Throw
import Hedgehog
import Hedgehog.Function
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Throw" $
  [ testGroup "ThrowC" $ throwTests ThrowC.runThrow
  ] where
  throwTests :: Has (Throw E) sig m => (forall a . m a -> PureC (Either E a)) -> [TestTree]
  throwTests run = Throw.throwTests run (genM (gen e)) e a b


gen :: (Has (Throw e) sig m, Show e) => Gen e -> (forall a . Show a => Gen a -> Gen (With (m a))) -> Gen a -> Gen (With (m a))
gen e _ _ = liftWith "throwError" throwError . showing <$> e


throwTests :: forall e m a b sig . (Has (Throw e) sig m, Arg a, Eq b, Eq e, Show a, Show b, Show e, Vary a) => (forall a . m a -> PureC (Either e a)) -> (forall a . Show a => Gen a -> Gen (With (m a))) -> Gen e -> Gen a -> Gen b -> [TestTree]
throwTests runThrow m e _ b =
  [ testProperty "throwError annihilates >>=" . forall (e :. fn @a (m b) :. Nil) $
    \ e (FnWith k) -> runThrow (throwError e >>= k) === runThrow (throwError e)
  ]
