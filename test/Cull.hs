{-# LANGUAGE FlexibleContexts, RankNTypes #-}
-- GHC 8.2.2 warns that the Has Cull sig m constraint on gen0 is redundant, but doesn’t typecheck without it. Newer GHCs typecheck just fine either way and also don’t warn, so … whatever?
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Cull
( tests
, gen0
, genN
, test
) where

import qualified Control.Carrier.Cull.Church as CullC
import Control.Effect.Choose
import Control.Effect.Cull
import Control.Effect.NonDet (NonDet)
import Gen
import qualified Monad
import qualified MonadFix
import qualified NonDet
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Cull"
  [ testGroup "CullC" $
    [ testMonad
    , testMonadFix
    , testCull
    ] >>= ($ runL CullC.runCullA)
  ] where
  testMonad    run = Monad.test    (m gen0 genN) a b c initial run
  testMonadFix run = MonadFix.test (m gen0 genN) a b   initial run
  testCull     run = Cull.test     (m gen0 genN) a b   initial run
  initial = identity <*> unit


gen0 :: (Has Cull sig m, Has NonDet sig m) => GenTerm a -> [GenTerm (m a)]
gen0 = NonDet.gen0

genN :: (Has Cull sig m, Has NonDet sig m) => GenM m -> GenTerm a -> [GenTerm (m a)]
genN m a = subtermM (m a) (label "cull" cull <*>) : NonDet.genN m a


test
  :: (Has Cull sig m, Has NonDet sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a, Functor f)
  => GenM m
  -> GenTerm a
  -> GenTerm b
  -> GenTerm (f ())
  -> Run f [] m
  -> [TestTree]
test m a b i (Run runCull)
  = testProperty "cull returns at most one success" (forall (i :. a :. m a :. m a :. Nil)
    (\ i a m n -> runCull ((cull (pure a <|> m) <|> n) <$ i) === runCull ((pure a <|> n) <$ i)))
  : NonDet.test m a b i (Run runCull)
