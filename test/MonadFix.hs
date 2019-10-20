{-# LANGUAGE RankNTypes #-}
module MonadFix
( test
) where

import Control.Monad.Fix
import Gen
import Test.Tasty
import Test.Tasty.Hedgehog

test
  :: (MonadFix m, Arg a, Eq (g a), Functor f, Show a, Show (g a), Vary a)
  => GenM m
  -> Gen a
  -> Gen (f ())
  -> Run f g m
  -> [TestTree]
test _ a s (Run run) =
  [ testProperty "purity" . forall (s :. fn a :. Nil) $
    \ s h -> run (mfix (return . h) <$ s) === run (return (fix h) <$ s)
  ]
