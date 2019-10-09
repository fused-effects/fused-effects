{-# OPTIONS_GHC -Wno-orphans #-}
module Pure
( module Control.Carrier.Pure
) where

import Control.Carrier.Pure
import Test.Tasty.QuickCheck

instance Arbitrary1 PureC where
  liftArbitrary genA = PureC <$> genA
  liftShrink shrinkA = map PureC . shrinkA . run

instance Arbitrary a => Arbitrary (PureC a) where
  arbitrary = arbitrary1
  shrink = shrink1
