{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Empty
( tests
, gen
, test
) where

import qualified Control.Carrier.Empty.Maybe as EmptyC
import Control.Effect.Empty
import Data.Maybe (maybeToList)
import Gen
import qualified Monad
import qualified MonadFix
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Empty"
  [ testGroup "EmptyC" $
    [ testMonad
    , testMonadFix
    , testEmpty
    ] >>= ($ RunL (fmap maybeToList . EmptyC.runEmpty))
  , testGroup "Maybe"  $ testEmpty (RunL (pure . maybeToList))
  ] where
  testMonad    run = Monad.test    (m gen) a b c (identity <*> unit) run
  testMonadFix run = MonadFix.test (m gen) a b   (identity <*> unit) run
  testEmpty    run = Empty.test    (m gen) a b                       run


gen :: Has Empty sig m => GenM m -> GenM m
gen _ _ = label "empty" empty


test
  :: forall a b m sig
  .  (Has Empty sig m, Arg a, Eq b, Show a, Show b, Vary a)
  => GenM m
  -> Gen a
  -> Gen b
  -> RunL [] m
  -> [TestTree]
test m _ b (RunL runEmpty) =
  [ testProperty "empty annihilates >>=" . forall (fn @a (m b) :. Nil) $
    \ k -> runEmpty (empty >>= k) === runEmpty empty
  ]
