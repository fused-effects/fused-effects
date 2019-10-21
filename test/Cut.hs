{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Cut
( tests
, gen
, test
) where

import qualified Control.Carrier.Cut.Church as CutC
import Control.Carrier.Reader
import Control.Effect.Choose
import Control.Effect.Cut (Cut, call, cutfail)
import Control.Effect.NonDet (NonDet)
import Gen
import qualified Monad
-- import qualified MonadFix
import qualified NonDet
import qualified Reader
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Cut"
  [ testGroup "CutC" $
    [ testMonad
    -- , testMonadFix
    , testCut
    ] >>= ($ runL CutC.runCutA)
  , testGroup "ReaderC · CutC" $
    Cut.test (Hom (local (id @R))) (m genCutReader) a b (pair <*> r <*> unit) (Run (CutC.runCutA . uncurry runReader))
  , testGroup "CutC · ReaderC" $
    Cut.test (Hom (local (id @R))) (m genCutReader) a b (pair <*> r <*> unit) (Run (uncurry ((. CutC.runCutA) . runReader)))
  ] where
  testMonad    run = Monad.test        (m gen) a b c (identity <*> unit) run
  -- testMonadFix run = MonadFix.test     (m gen) a b   (identity <*> unit) run
  testCut      run = Cut.test (Hom id) (m gen) a b   (identity <*> unit) run
  genCutReader m = GenM $ \ a -> choice [runGenM (gen m) a, runGenM (Reader.gen r m) a]


gen :: (Has Cut sig m, Has NonDet sig m) => GenM m -> GenM m
gen (GenM m) = GenM $ \ a -> choice
  [ label "call" call <*> m a
  , label "cutfail" cutfail
  , runGenM (NonDet.gen (GenM m)) a
  ]


newtype Hom m = Hom (forall a . m a -> m a)

test
  :: forall a b m f sig
  .  (Has Cut sig m, Has NonDet sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a, Functor f)
  => Hom m
  -> GenM m
  -> Gen a
  -> Gen b
  -> Gen (f ())
  -> Run f [] m
  -> [TestTree]
test (Hom hom) (GenM m) a b i (Run runCut)
  = testProperty "cutfail annihilates >>=" (forall (i :. fn @a (m a) :. Nil)
    (\ i k -> runCut ((hom cutfail >>= k) <$ i) === runCut (hom cutfail <$ i)))
  : testProperty "cutfail annihilates <|>" (forall (i :. m a :. Nil)
    (\ i m -> runCut ((hom cutfail <|> m) <$ i) === runCut (hom cutfail <$ i)))
  : testProperty "call delimits cutfail" (forall (i :. m a :. Nil)
    (\ i m -> runCut ((hom (call (hom cutfail)) <|> m) <$ i) === runCut (m <$ i)))
  : NonDet.test (GenM m) a b i (Run runCut)
