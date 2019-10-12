{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Reader.Function
( tests
) where

import Control.Carrier.Reader.Function
import Pure
import Test.QuickCheck.Poly
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Reader.Function"
  [ testProperty "ask environment" $
    \ a k -> ask_environment @A ((~=) @B) runReader a (getBlind k)
  ]

(~=) :: (Eq a, Show a) => PureC a -> PureC a -> Property
m1 ~= m2 = run m1 === run m2


instance (Arbitrary1 m, CoArbitrary r) => Arbitrary1 (ReaderC r m) where
  liftArbitrary genA = ReaderC <$> liftArbitrary @((->) r) (liftArbitrary @m genA)
  liftShrink shrinkA = map ReaderC . liftShrink (liftShrink shrinkA) . runReaderC

instance (Arbitrary1 m, Arbitrary a, CoArbitrary r) => Arbitrary (ReaderC r m a) where
  arbitrary = arbitrary1
  shrink = shrink1
