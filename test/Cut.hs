{-# LANGUAGE RankNTypes #-}
module Cut
( tests
, gen
) where

import Control.Effect.Cut
import Control.Effect.NonDet
import Hedgehog
import Hedgehog.Gen
import qualified NonDet
import Test.Tasty

tests :: TestTree
tests = testGroup "Cut"
  []


gen :: (Has Cut sig m, Has NonDet sig m) => (forall a . Gen a -> Gen (m a)) -> Gen a -> Gen (m a)
gen m a = choice
  [ subterm (m a) call
  , pure cutfail
  , NonDet.gen m a
  ]
