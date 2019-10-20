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
    ] >>= ($ runL CutC.runCutA)
  ] where
  testMonad    run = Monad.test    (m gen) a b c (identity <*> unit) run
  -- testMonadFix run = MonadFix.test (m gen) a b   (identity <*> unit) run
  testCut      run = Cut.test      (m gen) a b   (identity <*> unit) run


gen :: (Has Cut sig m, Has NonDet sig m) => GenM m -> GenM m
gen m a = choice
  [ label "call" call <*> m a
  , label "cutfail" cutfail
  , NonDet.gen m a
  ]


test
  :: forall a b m f sig
  .  (Has Cut sig m, Has NonDet sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a, Functor f)
  => GenM m
  -> Gen a
  -> Gen b
  -> Gen (f ())
  -> Run f [] m
  -> [TestTree]
test m a b i (Run runCut)
  = testProperty "cutfail annihilates >>=" (forall (i :. fn @a (m a) :. Nil)
    (\ i k -> runCut ((cutfail >>= k) <$ i) === runCut (cutfail <$ i)))
  : testProperty "cutfail annihilates <|>" (forall (i :. m a :. Nil)
    (\ i m -> runCut ((cutfail <|> m) <$ i) === runCut (cutfail <$ i)))
  : testProperty "call delimits cutfail" (forall (i :. m a :. Nil)
    (\ i m -> runCut ((call cutfail <|> m) <$ i) === runCut (m <$ i)))
  : NonDet.test m a b i (Run runCut)
