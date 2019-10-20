{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Empty
( tests
, gen
, test
) where

import qualified Control.Carrier.Empty.Maybe as EmptyC
import Control.Effect.Empty
import Data.Functor.Identity (Identity(..))
import Data.Maybe (maybeToList)
import Gen
import qualified Monad
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Empty"
  [ testGroup "EmptyC" $
    [ testMonad
    , testEmpty
    ] >>= ($ RunND (fmap maybeToList . EmptyC.runEmpty))
  , testGroup "Maybe"  $ testEmpty (RunND (pure . maybeToList))
  ] where
  testMonad (RunND run) = Monad.test (m gen) a b c (pure (Identity ())) (liftRunL run)
  testEmpty run         = Empty.test (m gen) a b                                  run


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
  -> RunND m
  -> [TestTree]
test m _ b (RunND runEmpty) =
  [ testProperty "empty annihilates >>=" . forall (fn @a (m b) :. Nil) $
    \ k -> runEmpty (empty >>= k) === runEmpty empty
  ]
