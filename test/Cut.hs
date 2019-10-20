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
import Data.Functor.Identity (Identity(..))
import Gen
import qualified Monad
import qualified NonDet
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Cut"
  [ testGroup "CutC" $
    [ testMonad
    , testCut
    ] >>= ($ RunND CutC.runCutA)
  ] where
  testMonad (RunND run) = Monad.test (m gen) a b c (pure (Identity ())) (run . runIdentity)
  testCut   run         = Cut.test   (m gen) a b                         run


gen
  :: (Has Cut sig m, Has NonDet sig m)
  => (forall a . Gen a -> Gen (m a))
  -> Gen a
  -> Gen (m a)
gen m a = choice
  [ label "call" call <*> m a
  , label "cutfail" cutfail
  , NonDet.gen m a
  ]


test
  :: forall a b m sig
  .  (Has Cut sig m, Has NonDet sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a)
  => (forall a . Gen a -> Gen (m a))
  -> Gen a
  -> Gen b
  -> RunND m
  -> [TestTree]
test m a b (RunND runCut)
  = testProperty "cutfail annihilates >>=" (forall (fn @a (m a) :. Nil)
    (\ k -> runCut (cutfail >>= k) === runCut cutfail))
  : testProperty "cutfail annihilates <|>" (forall (m a :. Nil)
    (\ m -> runCut (cutfail <|> m) === runCut cutfail))
  : testProperty "call delimits cutfail" (forall (m a :. Nil)
    (\ m -> runCut (call cutfail <|> m) === runCut m))
  : NonDet.test m a b (RunND runCut)
