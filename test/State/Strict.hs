{-# LANGUAGE FlexibleContexts #-}
module State.Strict
( tests
, gen
) where

import Control.Carrier
import Control.Carrier.State.Strict
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Test.Tasty

tests :: TestTree
tests = testGroup "State.Strict.StateC"
  []


gen :: (Carrier sig m, Effect sig) => Gen a -> Gen (StateC a m a)
gen a = Gen.choice [ pure get, put' <$> a, pure <$> a ] where
  put' a = a <$ put a
