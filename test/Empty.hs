{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Empty
( tests
, gen
, test
) where

import qualified Control.Carrier.Empty.Maybe as EmptyC
import Control.Effect.Empty
import Data.Functor.Identity (Identity(..))
import Gen
import qualified Monad
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Empty"
  [ test "EmptyC" EmptyC.runEmpty
  , test "Maybe"  pure
  ] where
  test :: Has Empty sig m => String -> (forall a . m a -> PureC (Maybe a)) -> TestTree
  test name run = testGroup name
    $  Monad.test (m gen) a b c (pure (Identity ())) (run . runIdentity)
    ++ Empty.test (m gen) a b                        run


gen
  :: Has Empty sig m
  => (forall a . Gen a -> Gen (m a))
  -> Gen a
  -> Gen (m a)
gen _ _ = label "empty" empty


test
  :: forall a b m sig
  .  (Has Empty sig m, Arg a, Eq b, Show a, Show b, Vary a)
  => (forall a . Gen a -> Gen (m a))
  -> Gen a
  -> Gen b
  -> (forall a . m a -> PureC (Maybe a))
  -> [TestTree]
test m _ b runEmpty =
  [ testProperty "empty annihilates >>=" . forall (fn @a (m b) :. Nil) $
    \ k -> runEmpty (empty >>= k) === runEmpty empty
  ]
