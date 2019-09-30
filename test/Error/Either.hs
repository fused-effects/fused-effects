{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Error.Either
( tests
) where

import Control.Carrier.Error.Either
import Control.Monad.Trans.Except
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Error.Either"
  []


instance (Arbitrary e, Arbitrary1 m, Arbitrary a) => Arbitrary (ErrorC e m a) where
  arbitrary = arbitrary1
  shrink = shrink1

instance (Arbitrary e, Arbitrary1 m) => Arbitrary1 (ErrorC e m) where
  liftArbitrary genA = ErrorC . ExceptT <$> liftArbitrary @m (liftArbitrary2 @Either (arbitrary @e) genA)
  liftShrink shrinkA = map (ErrorC . ExceptT) . liftShrink (liftShrink2 shrink shrinkA) . runError
