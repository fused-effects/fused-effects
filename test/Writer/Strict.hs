{-# LANGUAGE FlexibleContexts #-}
module Writer.Strict
( tests
, gen
) where

import Control.Carrier
import Control.Carrier.Writer.Strict
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Test.Tasty

tests :: TestTree
tests = testGroup "Writer.Strict.WriterC"
  []


gen :: (Carrier sig m, Effect sig, Monoid a) => Gen a -> Gen (WriterC a m a)
gen a = Gen.choice [ tell' <$> a, pure <$> a ] where
  tell' a = a <$ tell a
