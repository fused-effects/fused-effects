{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Cull
( tests
, gen
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
  testMonad    run = Monad.test    (m gen) a b c (identity <*> unit) run
  testMonadFix run = MonadFix.test (m gen) a b   (identity <*> unit) run
  testCull     run = Cull.test     (m gen) a b   (identity <*> unit) run


gen :: (Has Cull sig m, Has NonDet sig m) => GenM m -> GenM m
gen (GenM m) = GenM $ \ a -> choice
  [ label "cull" cull <*> m a
  , runGenM (NonDet.gen (GenM m)) a
  ]


test
  :: (Has Cull sig m, Has NonDet sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a, Functor f)
  => GenM m
  -> Gen a
  -> Gen b
  -> Gen (f ())
  -> Run f [] m
  -> [TestTree]
test (GenM m) a b i (Run runCull)
  = testProperty "cull returns at most one success" (forall (i :. a :. m a :. m a :. Nil)
    (\ i a m n -> runCull ((cull (pure a <|> m) <|> n) <$ i) === runCull ((pure a <|> n) <$ i)))
  : NonDet.test (GenM m) a b i (Run runCull)
