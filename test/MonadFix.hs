{-# LANGUAGE RankNTypes #-}
module MonadFix
( test
) where

import Gen
import Test.Tasty

test
  :: GenM m
  -> Gen a
  -> Gen (f ())
  -> Run f g m
  -> [TestTree]
test _ _ _ (Run _) =
  []
