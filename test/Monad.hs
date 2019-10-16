{-# LANGUAGE RankNTypes #-}
module Monad
( test
) where

import Control.Monad ((>=>))
import Gen
import Test.Tasty
import Test.Tasty.Hedgehog

test
  :: (Monad m, Arg a, Arg b, Eq a, Eq b, Eq c, Show a, Show b, Show c, Vary a, Vary b)
  => (forall a . Show a => Gen a -> Gen (With (m a)))
  -> Gen a
  -> Gen b
  -> Gen c
  -> (forall a . m a -> PureC a)
  -> [TestTree]
test m a b c run =
  [ testProperty "return is the left-identity of >>=" . forall (a :. fn (m b) :. Nil) $
    \ a (FnWith k) -> run (return a >>= k) === run (k a)
  , testProperty "return is the right-identity of >>=" . forall (m a :. Nil) $
    \ (With m) -> run (m >>= return) === run m
  , testProperty ">>= is associative" . forall (m a :. fn (m b) :. fn (m c) :. Nil) $
    \ (With m) (FnWith k) (FnWith h) -> run (m >>= (k >=> h)) === run ((m >>= k) >>= h)
  , testProperty "return = pure" . forall (a :. Nil) $
    \ a -> run (return a) === run (pure a)
  ]
