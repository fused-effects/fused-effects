{-# LANGUAGE RankNTypes #-}
module MonadFix
( test
) where

import Control.Monad.Fix
import Gen
import Test.Tasty
import Test.Tasty.Hedgehog

test
  :: (MonadFix m, Arg a, Eq (g a), Eq (g b), Functor f, Show a, Show (g a), Show (g b), Vary a)
  => GenM m
  -> Gen a
  -> Gen b
  -> Gen (f ())
  -> Run f g m
  -> [TestTree]
test m a b s (Run run) =
  [ testProperty "purity" . forall (s :. termFn a :. Nil) $
    \ s h -> run (mfix (return . h) <$ s) === run (return (fix h) <$ s)
  , testProperty "left-shrinking" . forall (s :. m a :. termFn (fn (m b)) :. Nil) $
    \ s a f -> run (mfix (\ x -> a >>= \ y -> f x y) <$ s) === run ((a >>= \ y -> mfix (\ x -> f x y)) <$ s)
  ]
