{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module State.Strict
( tests
, gen
) where

import Control.Carrier
import Control.Carrier.State.Strict
import Hedgehog
import Hedgehog.Function
import qualified Hedgehog.Gen as Gen
import Pure hiding (gen)
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "State.Strict.StateC"
  [ testProperty "get state" . forall (genA :. fn @A (Blind <$> gen genA) :. Nil) $
    \ a k -> get_state (~=) runState a (getBlind . apply k)
  ]

(~=) :: (Eq a, Show a) => PureC a -> PureC a -> PropertyT IO ()
m1 ~= m2 = run m1 === run m2


gen :: (Carrier sig m, Effect sig) => Gen a -> Gen (StateC a m a)
gen a = Gen.choice [ pure get, put' <$> a, pure <$> a ] where
  put' a = a <$ put a
