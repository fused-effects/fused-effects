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
import Data.Functor.Identity (Identity(..))
import Gen
import qualified Monad
import qualified NonDet
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Cull"
  [ testGroup "CullC" $
    [ testMonad
    , testCull
    ] >>= ($ RunL CullC.runCullA)
  ] where
  testMonad run = Monad.test (m gen) a b c (pure (Identity ())) run
  testCull  run = Cull.test  (m gen) a b                        run


gen
  :: (Has Cull sig m, Has NonDet sig m)
  => (forall a . Gen a -> Gen (m a))
  -> Gen a
  -> Gen (m a)
gen m a = choice
  [ label "cull" cull <*> m a
  , NonDet.gen m a
  ]


test
  :: (Has Cull sig m, Has NonDet sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a)
  => (forall a . Gen a -> Gen (m a))
  -> Gen a
  -> Gen b
  -> RunL [] m
  -> [TestTree]
test m a b (RunL runCull)
  = testProperty "cull returns at most one success" (forall (a :. m a :. m a :. Nil)
    (\ a m n -> runCull (cull (pure a <|> m) <|> n) === runCull (pure a <|> n)))
  : NonDet.test m a b (RunL runCull)
