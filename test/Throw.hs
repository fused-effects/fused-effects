{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Throw
( tests
, gen0
, test
) where

import qualified Control.Carrier.Throw.Either as ThrowC
import Control.Effect.Throw
import Gen
import qualified Monad
import qualified MonadFix
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Throw" $
  [ testGroup "ThrowC" $
    [ testMonad
    , testMonadFix
    , testThrow
    ] >>= ($ runL ThrowC.runThrow)
  ] where
  testMonad    run = Monad.test    (m (gen0 e) (\ _ _ -> [])) a b c initial run
  testMonadFix run = MonadFix.test (m (gen0 e) (\ _ _ -> [])) a b   initial run
  testThrow    run = Throw.test e  (m (gen0 e) (\ _ _ -> [])) a b   initial run
  initial = identity <*> unit


gen0 :: Has (Throw e) sig m => GenTerm e -> GenTerm a -> [GenTerm (m a)]
gen0 e _ = [ label "throwError" throwError <*> e ]


test
  :: forall e m a b f sig
  .  (Has (Throw e) sig m, Arg a, Eq b, Eq e, Show a, Show b, Show e, Vary a, Functor f)
  => GenTerm e
  -> GenM m
  -> GenTerm a
  -> GenTerm b
  -> GenTerm (f ())
  -> Run f (Either e) m
  -> [TestTree]
test e m _ b i (Run runThrow) =
  [ testProperty "throwError annihilates >>=" . forall (i :. e :. fn @a (m b) :. Nil) $
    \ i e k -> runThrow ((throwError e >>= k) <$ i) === runThrow ((throwError e) <$ i)
  ]
