{-# LANGUAGE RankNTypes #-}
module Reader
( gen
, testReader
) where

import Control.Effect.Reader
import Pure
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen
import Test.Tasty
import Test.Tasty.Hedgehog

gen :: (Has (Reader a) sig m, Arg a, Vary a) => Gen a -> Gen (m a)
gen a = choice
  [ pure ask
  , fn a >>= subterm (gen a) . local . apply
  , pure <$> a
  ]

testReader :: (Has (Reader r) sig m, Arg r, Eq r, Show r, Vary r) => String -> (forall a . r -> m a -> PureC a) -> Gen r -> TestTree
testReader name runReader genA = testGroup name
  [ testProperty "ask environment" . forall (genA :. fn (Blind <$> gen genA) :. Nil) $
    \ a k -> ask_environment (~=) runReader a (getBlind . apply k)
  , testProperty "local modification" . forall (genA :. fn genA :. fmap Blind (gen genA) :. Nil) $
    \ a f m -> local_modification (~=) runReader a (apply f) (getBlind m)
  ]
