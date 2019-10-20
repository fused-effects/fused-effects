{-# LANGUAGE RankNTypes #-}
module MonadFix
( test
) where

import Control.Monad.Fix
import Gen
import Test.Tasty
import Test.Tasty.Hedgehog

test
  :: (MonadFix m, Eq (g a), Functor f, Show (g a))
  => GenM m
  -> Gen a
  -> Gen (f ())
  -> Run f g m
  -> [TestTree]
test _ a s (Run run) =
  [ testProperty "purity" . forall (s :. termFn a :. Nil) $
    \ s h -> run (mfix (return . h) <$ s) === run (return (fix h) <$ s)
  ]
