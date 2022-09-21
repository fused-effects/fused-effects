{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Fresh
( tests
, gen
, test
) where

import qualified Control.Carrier.Fresh.Church as C.Church
import qualified Control.Carrier.Fresh.Strict as C.Strict
import           Control.Effect.Fresh
import           Gen
import qualified Hedgehog.Range as R
import qualified Monad
import qualified MonadFix

tests :: TestTree
tests = testGroup "Fresh"
  [ testGroup "FreshC (Church)" $
    [ testMonad
    , testMonadFix
    , testFresh
    ] >>= ($ runC (C.Church.runFresh (curry pure)))
  , testGroup "FreshC (Strict)" $
    [ testMonad
    , testMonadFix
    , testFresh
    ] >>= ($ runC C.Strict.runFresh)
  ] where
  testMonad    run = Monad.test    (m gen (\ _ _ -> [])) a b c initial run
  testMonadFix run = MonadFix.test (m gen (\ _ _ -> [])) a b   initial run
  testFresh    run = Fresh.test    (m gen (\ _ _ -> [])) a     initial run
  initial = pair <*> n <*> unit
  n = Gen.integral (R.linear 0 100)


gen :: Has Fresh sig m => GenTerm a -> [GenTerm (m a)]
gen a = [ atom "fmap" fmap <*> fn a <*> label "fresh" fresh ]


test
  :: (Has Fresh sig m, Functor f)
  => GenM m
  -> GenTerm a
  -> GenTerm (f ())
  -> Run f ((,) Int) m
  -> [TestTree]
test m a i (Run runFresh) =
  [ testProperty "fresh yields unique values" . forall_ (i :. m a :. Nil) $
    \ i m -> runFresh ((m >> fresh) <$ i) /== runFresh ((m >> fresh >> fresh) <$ i)
  ]
