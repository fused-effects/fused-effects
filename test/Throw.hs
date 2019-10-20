{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Throw
( tests
, gen
, test
) where

import qualified Control.Carrier.Throw.Either as ThrowC
import Control.Effect.Throw
import Gen
import qualified Monad
import qualified MonadFix
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Throw" $
  [ testGroup "ThrowC" $
    [ testMonad
    , testMonadFix
    , testThrow
    ] >>= ($ RunL ThrowC.runThrow)
  ] where
  testMonad    run = Monad.test    (m (gen e)) a b c (identity <*> unit) run
  testMonadFix run = MonadFix.test (m (gen e)) a b   (identity <*> unit) run
  testThrow    run = Throw.test e  (m (gen e)) a b                       run


gen :: Has (Throw e) sig m => Gen e -> GenM m -> GenM m
gen e _ _ = label "throwError" throwError <*> e


test
  :: forall e m a b sig
  .  (Has (Throw e) sig m, Arg a, Eq b, Eq e, Show a, Show b, Show e, Vary a)
  => Gen e
  -> GenM m
  -> Gen a
  -> Gen b
  -> RunL (Either e) m
  -> [TestTree]
test e m _ b (RunL runThrow) =
  [ testProperty "throwError annihilates >>=" . forall (e :. fn @a (m b) :. Nil) $
    \ e k -> runThrow (throwError e >>= k) === runThrow (throwError e)
  ]
