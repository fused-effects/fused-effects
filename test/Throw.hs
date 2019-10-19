{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Throw
( tests
, gen
, test
) where

import qualified Control.Carrier.Throw.Either as ThrowC
import Control.Effect.Throw
import Gen
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Throw" $
  [ testGroup "ThrowC" $ test e (m (gen e)) a b ThrowC.runThrow
  ]


gen
  :: Has (Throw e) sig m
  => Gen e
  -> (forall a . Gen a -> Gen (m a))
  -> Gen a
  -> Gen (m a)
gen e _ _ = label "throwError" throwError <*> e


test
  :: forall e m a b sig
  .  (Has (Throw e) sig m, Arg a, Eq b, Eq e, Show a, Show b, Show e, Vary a)
  => Gen e
  -> (forall a . Gen a -> Gen (m a))
  -> Gen a
  -> Gen b
  -> (forall a . m a -> PureC (Either e a))
  -> [TestTree]
test e m _ b runThrow =
  [ testProperty "throwError annihilates >>=" . forall (e :. fn @a (m b) :. Nil) $
    \ e k -> runThrow (throwError e >>= k) === runThrow (throwError e)
  ]
