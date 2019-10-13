{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module State.StateT.Lazy
( tests
, gen
) where

import Control.Carrier
import Control.Effect.State
import qualified Control.Monad.Trans.State.Lazy as StateT
import Hedgehog
import Hedgehog.Function
import qualified Hedgehog.Gen as Gen
import Pure hiding (gen)
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "State.StateT.Lazy"
  [ testProperty "get state" . forall (genA :. fn @A (Blind <$> gen genA) :. Nil) $
    \ a k -> get_state (~=) (flip StateT.runStateT) a (getBlind . apply k)
  , testProperty "put update" . forall (genA :. genA :. fmap Blind (gen genA) :. Nil) $
    \ a b m -> put_update (~=) (flip StateT.runStateT) a b (getBlind m)
  ]

(~=) :: (Eq a, Show a) => PureC a -> PureC a -> PropertyT IO ()
m1 ~= m2 = run m1 === run m2


gen :: (Carrier sig m, Effect sig) => Gen a -> Gen (StateT.StateT a m a)
gen a = Gen.choice [ pure get, put' <$> a, pure <$> a ] where
  put' a = a <$ put a
