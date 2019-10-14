{-# LANGUAGE RankNTypes #-}
module Cull
( tests
, gen
) where

import Control.Effect.Cull
import Control.Effect.NonDet (NonDet)
import Hedgehog
import Hedgehog.Gen
import qualified NonDet
import Test.Tasty

tests :: TestTree
tests = testGroup "Cull"
  []


gen :: (Has Cull sig m, Has NonDet sig m) => (forall a . Gen a -> Gen (m a)) -> Gen a -> Gen (m a)
gen m a = choice
  [ subterm (m a) cull
  , NonDet.gen m a
  ]
