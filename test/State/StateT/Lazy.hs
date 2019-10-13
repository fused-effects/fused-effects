{-# LANGUAGE TypeApplications #-}
module State.StateT.Lazy
( tests
) where

import Control.Carrier
import Control.Effect.State
import qualified Control.Monad.Trans.State.Lazy as StateT
import Hedgehog
import Hedgehog.Function
import Pure
import qualified State
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "State.StateT.Lazy"
  [ testProperty "get state" . forall (genA :. fn @A (Blind <$> State.gen genA) :. Nil) $
    \ a k -> get_state (~=) (flip StateT.runStateT) a (getBlind . apply k)
  , testProperty "put update" . forall (genA :. genA :. fmap Blind (State.gen genA) :. Nil) $
    \ a b m -> put_update (~=) (flip StateT.runStateT) a b (getBlind m)
  ]

(~=) :: (Eq a, Show a) => PureC a -> PureC a -> PropertyT IO ()
m1 ~= m2 = run m1 === run m2
