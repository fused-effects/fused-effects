{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Throw
( tests
, gen0
, test
) where

import qualified Control.Carrier.Throw.Either as ThrowC
import           Control.Effect.Throw
import           Gen
import qualified Monad
import qualified MonadFix

tests :: TestTree
tests = testGroup "Throw"
  [ testGroup "ThrowC" $
    [ testMonad
    , testMonadFix
    , testThrow
    ] >>= ($ runL ThrowC.runThrow)
  ] where
  testMonad    run = Monad.test       (genM (gen0 termE) (\ _ _ -> [])) termA termB termC initial run
  testMonadFix run = MonadFix.test    (genM (gen0 termE) (\ _ _ -> [])) termA termB       initial run
  testThrow    run = Throw.test termE (genM (gen0 termE) (\ _ _ -> [])) termA termB       initial run
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
  [ testProperty "throwError annihilates >>=" . forall_ (i :. e :. fn @a (m b) :. Nil) $
    \ i e k -> runThrow ((throwError e >>= k) <$ i) === runThrow (throwError e <$ i)
  ]
