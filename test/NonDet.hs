{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module NonDet
( tests
, gen0
, genN
, test
) where

import qualified Choose
import qualified Control.Carrier.NonDet.Church as Church.NonDetC
import Control.Effect.Choose
import Control.Effect.Empty
import Control.Effect.NonDet (NonDet)
import qualified Empty
import Gen
import qualified Monad
import qualified MonadFix
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "NonDet"
  [ testGroup "NonDetC (Church)" $
    [ testMonad
    , testMonadFix
    , testNonDet
    ] >>= ($ runL Church.NonDetC.runNonDetA)
  , testGroup "[]" $ testNonDet (runL pure)
  ] where
  testMonad    run = Monad.test    (m (const gen0) genN) a b c (identity <*> unit) run
  testMonadFix run = MonadFix.test (m (const gen0) genN) a b   (identity <*> unit) run
  testNonDet   run = NonDet.test   (m (const gen0) genN) a b   (identity <*> unit) run


gen0 :: Has NonDet sig m => [Gen (m a)]
gen0 = Empty.gen0

genN :: Has NonDet sig m => GenM m -> Gen a -> [Gen (m a)]
genN = Choose.genN


test
  :: (Has NonDet sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a, Functor f)
  => GenM m
  -> Gen a
  -> Gen b
  -> Gen (f ())
  -> Run f [] m
  -> [TestTree]
test m a b i (Run runNonDet)
  =  testProperty "empty is the left identity of <|>"  (forall (i :. m a :. Nil)
    (\ i m -> runNonDet ((empty <|> m) <$ i) === runNonDet (m <$ i)))
  :  testProperty "empty is the right identity of <|>" (forall (i :. m a :. Nil)
    (\ i m -> runNonDet ((m <|> empty) <$ i) === runNonDet (m <$ i)))
  :  Empty.test  m a b i (Run runNonDet)
  ++ Choose.test m a b i (Run runNonDet)
