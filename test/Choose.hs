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
    ] >>= ($ runL (ChooseC.runChooseS (pure . pure)))
  , testGroup "NonEmpty" $ testChoose (runL (pure . toList))
  ] where
  testMonad    run = Monad.test    (m gen) a b c (identity <*> unit) run
  testMonadFix run = MonadFix.test (m gen) a b   (identity <*> unit) run
  testChoose   run = Choose.test   (m gen) a b   (identity <*> unit) run


gen :: Has Choose sig m => GenM m -> GenM m
gen (GenM m) = GenM $ \ a -> addLabel "<|>" (infixL 3 "<|>" (<|>) <*> m a <*> m a)


test
  :: (Has Choose sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a, Functor f)
  => GenM m
  -> Gen a
  -> Gen b
  -> Gen (f ())
  -> Run f [] m
  -> [TestTree]
test (GenM m) a b i (Run runChoose) =
  [ testProperty ">>= distributes over <|>" . forall (i :. m a :. m a :. fn (m b) :. Nil) $
    \ i m n k -> runChoose (((m <|> n) >>= k) <$ i) === runChoose (((m >>= k) <|> (n >>= k)) <$ i)
  , testProperty "<|> is associative" . forall (i :. m a :. m a :. m a :. Nil) $
    \ i m n o -> runChoose (((m <|> n) <|> o) <$ i) === runChoose ((m <|> (n <|> o)) <$ i)
  ]
