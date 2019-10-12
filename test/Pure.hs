{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Pure
( module Control.Carrier.Pure
, gen
, genA
, genB
, genC
, A(..)
, B(..)
, C(..)
) where

import Control.Carrier.Pure
import Hedgehog
import qualified Hedgehog.Function as Function
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.QuickCheck.Poly
import Test.Tasty.QuickCheck hiding (Gen)

instance Arbitrary1 PureC where
  liftArbitrary genA = PureC <$> genA
  liftShrink shrinkA = map PureC . shrinkA . run

instance Arbitrary a => Arbitrary (PureC a) where
  arbitrary = arbitrary1
  shrink = shrink1


gen :: Gen a -> Gen (PureC a)
gen = fmap PureC

genA :: Gen A
genA = A <$> Gen.integral (Range.linear 0 10)

genB :: Gen B
genB = B <$> Gen.integral (Range.linear 0 10)

genC :: Gen C
genC = C <$> Gen.integral (Range.linear 0 10)

instance Function.Arg A
instance Function.Arg B
instance Function.Arg C
deriving instance Function.Generic A
deriving instance Function.Generic B
deriving instance Function.Generic C
deriving instance Function.Vary A
deriving instance Function.Vary B
deriving instance Function.Vary C
