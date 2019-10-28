{-# LANGUAGE RankNTypes #-}
module Monad
( test
) where

import Control.Monad ((>=>), ap)
import Gen
import Test.Tasty
import Test.Tasty.Hedgehog

test
  :: (Monad m, Arg a, Arg b, Eq (g a), Eq (g b), Eq (g c), Show a, Show b, Show (g a), Show (g b), Show (g c), Vary a, Vary b, Functor f)
  => GenM m
  -> GenTerm a
  -> GenTerm b
  -> GenTerm c
  -> GenTerm (f ())
  -> Run f g m
  -> [TestTree]
test m a b c s (Run run) =
  [ testProperty "return is the left-identity of >>=" . forall (s :. a :. fn (m b) :. Nil) $
    \ s a k -> run ((return a >>= k) <$ s) === run ((k a) <$ s)
  , testProperty "return is the right-identity of >>=" . forall (s :. m a :. Nil) $
    \ s m -> run ((m >>= return) <$ s) === run (m <$ s)
  , testProperty ">>= is associative" . forall (s :. m a :. fn (m b) :. fn (m c) :. Nil) $
    \ s m k h -> run ((m >>= (k >=> h)) <$ s) === run (((m >>= k) >>= h) <$ s)
  , testProperty "return = pure" . forall (s :. a :. Nil) $
    \ s a -> run (return a <$ s) === run (pure a <$ s)
  , testProperty "ap = (<*>)" . forall (s :. fn b :. m a :. Nil) $
    \ s f m -> run ((pure f `ap` m) <$ s) === run ((pure f <*> m) <$ s)
  ]
