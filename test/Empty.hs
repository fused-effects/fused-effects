{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Empty
( tests
, gen
, test
) where

import qualified Control.Carrier.Empty.Maybe as EmptyC
import Control.Effect.Empty
import Gen
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Empty"
  [ testGroup "EmptyC" $ test (m gen) a b EmptyC.runEmpty
  , testGroup "Maybe"  $ test (m gen) a b pure
  ]


gen
  :: Has Empty sig m
  => (forall a . Show a => Gen a -> Gen (With (m a)))
  -> Gen a
  -> Gen (With (m a))
gen _ _ = pure (atom "empty" empty)


test
  :: forall a b m sig
  .  (Has Empty sig m, Arg a, Eq b, Show a, Show b, Vary a)
  => (forall a. Show a => Gen a -> Gen (With (m a)))
  -> Gen a
  -> Gen b
  -> (forall a . m a -> PureC (Maybe a))
  -> [TestTree]
test m _ b runEmpty =
  [ testProperty "empty annihilates >>=" . forall (fn @a (m b) :. Nil) $
    \ (FnWith k) -> runEmpty (empty >>= k) === runEmpty empty
  ]
