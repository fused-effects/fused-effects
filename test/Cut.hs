{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Cut
( tests
, gen0
, genN
, test
) where

import qualified Control.Carrier.Cut.Church as CutC
import Control.Carrier.Reader
import Control.Effect.Choose
import Control.Effect.Cut (Cut, call, cutfail)
import Control.Effect.NonDet (NonDet)
import Data.Semigroup as S ((<>))
import Gen
import qualified Monad
import qualified MonadFix
import qualified NonDet
import qualified Reader
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Cut"
  [ testGroup "CutC" $
    [ testMonad
    , testMonadFix
    , testCut
    ] >>= ($ runL CutC.runCutA)
  , testGroup "ReaderC · CutC" $
    Cut.test (local (id @R)) (m (gen0 S.<> Reader.gen0 r) (\ m -> genN m S.<> Reader.genN r m)) a b (pair <*> r <*> unit) (Run (CutC.runCutA . uncurry runReader))
  , testGroup "CutC · ReaderC" $
    Cut.test (local (id @R)) (m (gen0 S.<> Reader.gen0 r) (\ m -> genN m S.<> Reader.genN r m)) a b (pair <*> r <*> unit) (Run (uncurry ((. CutC.runCutA) . runReader)))
  ] where
  testMonad    run = Monad.test    (m gen0 genN) a b c initial run
  testMonadFix run = MonadFix.test (m gen0 genN) a b   initial run
  testCut      run = Cut.test id   (m gen0 genN) a b   initial run
  initial = identity <*> unit


gen0 :: (Has Cut sig m, Has NonDet sig m) => GenTerm a -> [GenTerm (m a)]
gen0 a = label "cutfail" cutfail : NonDet.gen0 a

genN :: (Has Cut sig m, Has NonDet sig m) => GenM m -> GenTerm a -> [GenTerm (m a)]
genN m a = subtermM (m a) (label "call" call <*>) : NonDet.genN m a


test
  :: forall a b m f sig
  .  (Has Cut sig m, Has NonDet sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a, Functor f)
  => (forall a . m a -> m a)
  -> GenM m
  -> GenTerm a
  -> GenTerm b
  -> GenTerm (f ())
  -> Run f [] m
  -> [TestTree]
test hom m = (\ a _ i (Run runCut) ->
  [ testProperty "cutfail annihilates >>=" (forall (i :. fn @a (m a) :. Nil)
    (\ i k -> runCut ((hom cutfail >>= k) <$ i) === runCut (hom cutfail <$ i)))
  , testProperty "cutfail annihilates <|>" (forall (i :. m a :. Nil)
    (\ i m -> runCut ((hom cutfail <|> m) <$ i) === runCut (hom cutfail <$ i)))
  , testProperty "call delimits cutfail" (forall (i :. m a :. Nil)
    (\ i m -> runCut ((hom (call (hom cutfail)) <|> m) <$ i) === runCut (m <$ i)))
  ])
  S.<> NonDet.test m
