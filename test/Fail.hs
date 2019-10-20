{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Fail
( tests
, gen
, test
) where

import qualified Control.Carrier.Fail.Either as FailC
import Control.Effect.Fail as Fail
import Gen
import Hedgehog.Range as Range
import qualified Monad
import qualified MonadFix
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Fail" $
  [ testGroup "FailC" $
    [ testMonad
    , testMonadFix
    , testFail
    ] >>= ($ RunL FailC.runFail)
  ] where
  testMonad    run = Monad.test    (m (gen e)) a b c (identity <*> unit) run
  testMonadFix run = MonadFix.test (m (gen e)) a b   (identity <*> unit) run
  testFail     run = Fail.test e   (m (gen e)) a b                       run
  e = string (Range.linear 0 50) unicode


gen :: MonadFail m => Gen String -> GenM m -> GenM m
gen e _ _ = label "fail" Fail.fail <*> e


test
  :: forall m a b
  .  (MonadFail m, Arg a, Eq b, Show a, Show b, Vary a)
  => Gen String
  -> GenM m
  -> Gen a
  -> Gen b
  -> RunL (Either String) m
  -> [TestTree]
test msg m _ b (RunL runThrow) =
  [ testProperty "fail annihilates >>=" . forall (msg :. fn @a (m b) :. Nil) $
    \ s k -> runThrow (Fail.fail s >>= k) === runThrow (Fail.fail s)
  ]
