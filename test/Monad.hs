{-# LANGUAGE RankNTypes #-}
module Monad
( test
) where

import Gen
import Test.Tasty
import Test.Tasty.Hedgehog

test
  :: (Monad m, Arg a, Eq a, Eq b, Show a, Show b, Vary a)
  => (forall a . Show a => Gen a -> Gen (With (m a)))
  -> Gen a
  -> Gen b
  -> (forall a . m a -> PureC a)
  -> [TestTree]
test m a b run =
  [ testProperty "return is the left-identity of >>=" . forall (a :. fn (m b) :. Nil) $
    \ a (FnWith k) -> run (return a >>= k) === run (k a)
  , testProperty "return is the right-identity of >>=" . forall (m a :. Nil) $
    \ (With m) -> run (m >>= return) === run m
  ]
