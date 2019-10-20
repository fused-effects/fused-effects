{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Cut
( tests
, gen
, test
) where

import qualified Control.Carrier.Cut.Church as CutC
import Control.Effect.Choose
import Control.Effect.Cut (Cut, call, cutfail)
import Control.Effect.NonDet (NonDet)
import Gen
import qualified Monad
-- import qualified MonadFix
import qualified NonDet
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Cut"
  [ testGroup "CutC" $
    [ testMonad
    -- , testMonadFix
    , testCut
    ] >>= ($ RunL CutC.runCutA)
  ] where
  testMonad    run = Monad.test    (m gen) a b c (identity <*> unit) run
  -- testMonadFix run = MonadFix.test (m gen) a b   (identity <*> unit) run
  testCut      run = Cut.test      (m gen) a b                       run


gen :: (Has Cut sig m, Has NonDet sig m) => GenM m -> GenM m
gen m a = choice
  [ label "call" call <*> m a
  , label "cutfail" cutfail
  , NonDet.gen m a
  ]


test
  :: forall a b m sig
  .  (Has Cut sig m, Has NonDet sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a)
  => GenM m
  -> Gen a
  -> Gen b
  -> RunL [] m
  -> [TestTree]
test m a b (RunL runCut)
  = testProperty "cutfail annihilates >>=" (forall (fn @a (m a) :. Nil)
    (\ k -> runCut (cutfail >>= k) === runCut cutfail))
  : testProperty "cutfail annihilates <|>" (forall (m a :. Nil)
    (\ m -> runCut (cutfail <|> m) === runCut cutfail))
  : testProperty "call delimits cutfail" (forall (m a :. Nil)
    (\ m -> runCut (call cutfail <|> m) === runCut m))
  : NonDet.test m a b (RunL runCut)
