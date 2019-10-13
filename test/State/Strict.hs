{-# LANGUAGE TypeApplications #-}
module State.Strict
( tests
) where

import Control.Carrier
import Control.Carrier.State.Strict
import Hedgehog
import Hedgehog.Function
import Pure
import qualified State
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "State.Strict.StateC"
  [ testProperty "get state" . forall (genA :. fn @A (Blind <$> State.gen genA) :. Nil) $
    \ a k -> get_state (~=) runState a (getBlind . apply k)
  , testProperty "put update" . forall (genA :. genA :. fmap Blind (State.gen genA) :. Nil) $
    \ a b m -> put_update (~=) runState a b (getBlind m)
  ]

(~=) :: (Eq a, Show a) => PureC a -> PureC a -> PropertyT IO ()
m1 ~= m2 = run m1 === run m2
