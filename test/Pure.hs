module Pure
() where

import Control.Carrier.Pure
import Test.QuickCheck

instance Arbitrary a => Arbitrary (PureC a) where
  arbitrary = PureC <$> arbitrary
  shrink = map PureC . shrink . run
