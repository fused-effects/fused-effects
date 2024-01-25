{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Alternative law, left identity" #-}
{-# HLINT ignore "Alternative law, right identity" #-}
{-# HLINT ignore "Eta reduce" #-}
module NonDet
( tests
, gen0
, genN
, test
) where

import qualified Choose
import qualified Control.Carrier.NonDet.Church as Church.NonDetC
import           Control.Effect.Choose
import           Control.Effect.Empty
import           Control.Effect.NonDet (NonDet)
import           Data.Semigroup as S ((<>))
import qualified Empty
import           Gen
import qualified Monad
import qualified MonadFix

tests :: TestTree
tests = testGroup "NonDet"
  [ testGroup "NonDetC (Church)" $
    [ testMonad
    , testMonadFix
    , testNonDet
    ] >>= ($ runL Church.NonDetC.runNonDetA)
  , testGroup "[]" $ testNonDet (runL pure)
  ] where
  testMonad    run = Monad.test    (genM gen0 genN) termA termB termC initial run
  testMonadFix run = MonadFix.test (genM gen0 genN) termA termB       initial run
  testNonDet   run = NonDet.test   (genM gen0 genN) termA termB       initial run
  initial = identity <*> unit


gen0 :: Has NonDet sig m => GenTerm a -> [GenTerm (m a)]
gen0 = Empty.gen0

genN :: Has NonDet sig m => GenM m -> GenTerm a -> [GenTerm (m a)]
genN = Choose.genN


test
  :: (Has NonDet sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a, Functor f)
  => GenM m
  -> GenTerm a
  -> GenTerm b
  -> GenTerm (f ())
  -> Run f [] m
  -> [TestTree]
test m
  = (\ a _ i (Run runNonDet) ->
    [ testProperty "empty is the left identity of <|>"  (forall_ (i :. m a :. Nil)
      (\ i m -> runNonDet ((empty <|> m) <$ i) === runNonDet (m <$ i)))
    ,  testProperty "empty is the right identity of <|>" (forall_ (i :. m a :. Nil)
      (\ i m -> runNonDet ((m <|> empty) <$ i) === runNonDet (m <$ i)))
    ])
  S.<> Empty.test  m
  S.<> Choose.test m
