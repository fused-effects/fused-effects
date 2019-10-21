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
    ] >>= ($ runL FailC.runFail)
  ] where
  testMonad    run = Monad.test    (m (gen e)) a b c (identity <*> unit) run
  testMonadFix run = MonadFix.test (m (gen e)) a b   (identity <*> unit) run
  testFail     run = Fail.test e   (m (gen e)) a b   (identity <*> unit) run
  e = string (Range.linear 0 50) unicode


gen :: MonadFail m => Gen String -> GenM m -> GenM m
gen e _ = GenM $ \ _ -> label "fail" Fail.fail <*> e


test
  :: forall m a b f
  .  (MonadFail m, Arg a, Eq b, Show a, Show b, Vary a, Functor f)
  => Gen String
  -> GenM m
  -> Gen a
  -> Gen b
  -> Gen (f ())
  -> Run f (Either String) m
  -> [TestTree]
test msg (GenM m) _ b i (Run runFail) =
  [ testProperty "fail annihilates >>=" . forall (i :. msg :. fn @a (m b) :. Nil) $
    \ i s k -> runFail ((Fail.fail s >>= k) <$ i) === runFail ((Fail.fail s) <$ i)
  ]
