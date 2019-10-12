{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
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
, Rec(..)
, forall
, Blind(..)
) where

import Control.Carrier.Pure
import Hedgehog
import qualified Hedgehog.Function as Function
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.QuickCheck.Poly
import Test.Tasty.QuickCheck hiding (Blind, Gen)

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


infixr 5 :.

data Rec as where
  Nil :: Rec '[]
  (:.) :: a -> Rec as -> Rec (a ': as)

forall :: Forall g f => g -> f -> Hedgehog.Property
forall g f = Hedgehog.property (forall' g f)

class Forall g f | g -> f, f -> g where
  forall' :: g -> f -> PropertyT IO ()

instance Forall (Rec '[]) (PropertyT IO ()) where
  forall' Nil = id

instance (Forall (Rec gs) b, Show a) => Forall (Rec (Gen a ': gs)) (a -> b) where
  forall' (g :. gs) f = do
    a <- Hedgehog.forAll g
    forall' gs (f a)


newtype Blind a = Blind { getBlind :: a }
  deriving (Eq, Ord)

instance Show (Blind a) where
  show _ = "_"
