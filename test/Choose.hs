{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Choose
( tests
, gen
, test
) where

import qualified Control.Carrier.Choose.Church as ChooseC
import Control.Effect.Choose
import Data.List.NonEmpty
import Gen
import qualified Monad
import qualified MonadFix
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Choose"
  [ testGroup "ChooseC"  $
    [ testMonad
    , testMonadFix
    , testChoose
    ] >>= ($ RunL (ChooseC.runChooseS (pure . pure)))
  , testGroup "NonEmpty" $ testChoose (RunL (pure . toList))
  ] where
  testMonad    run = Monad.test    (m gen) a b c (identity <*> unit) run
  testMonadFix run = MonadFix.test (m gen) a b   (identity <*> unit) run
  testChoose   run = Choose.test   (m gen) a b                       run


gen :: Has Choose sig m => GenM m -> GenM m
gen m a = addLabel "<|>" (infixL 3 "<|>" (<|>) <*> m a <*> m a)


test
  :: (Has Choose sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a)
  => GenM m
  -> Gen a
  -> Gen b
  -> RunL [] m
  -> [TestTree]
test m a b (RunL runChoose) =
  [ testProperty ">>= distributes over <|>" . forall (m a :. m a :. fn (m b) :. Nil) $
    \ m n k -> runChoose ((m <|> n) >>= k) === runChoose ((m >>= k) <|> (n >>= k))
  , testProperty "<|> is associative" . forall (m a :. m a :. m a :. Nil) $
    \ m n o -> runChoose ((m <|> n) <|> o) === runChoose (m <|> (n <|> o))
  ]
