{-# LANGUAGE RankNTypes #-}
module Monad
( test
) where

import Control.Monad ((>=>), ap)
import Gen
import Test.Tasty
import Test.Tasty.Hedgehog

test
  :: (Monad m, Arg a, Arg b, Eq (f a), Eq (f b), Eq (f c), Show a, Show b, Show c, Show (f a), Show (f b), Show (f c), Vary a, Vary b)
  => (forall a . Show a => Gen a -> Gen (m a))
  -> Gen a
  -> Gen b
  -> Gen c
  -> (forall a . m a -> PureC (f a))
  -> [TestTree]
test m a b c run =
  [ testProperty "return is the left-identity of >>=" . forall (a :. fn (m b) :. Nil) $
    \ a k -> run (return a >>= k) === run (k a)
  , testProperty "return is the right-identity of >>=" . forall (m a :. Nil) $
    \ m -> run (m >>= return) === run m
  , testProperty ">>= is associative" . forall (m a :. fn (m b) :. fn (m c) :. Nil) $
    \ m k h -> run (m >>= (k >=> h)) === run ((m >>= k) >>= h)
  , testProperty "return = pure" . forall (a :. Nil) $
    \ a -> run (return a) === run (pure a)
  , testProperty "ap = (<*>)" . forall (fn b :. m a :. Nil) $
    \ f m -> run (pure f `ap` m) === run (pure f <*> m)
  ]
