{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-identities #-}
module Pure
( module Control.Carrier.Pure
, (~=)
, genM
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
import Hedgehog.Function hiding (C)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

(~=) :: (Eq a, Show a) => PureC a -> PureC a -> PropertyT IO ()
m1 ~= m2 = run m1 === run m2


genM :: forall m a . Applicative m => (forall a . (forall a . Gen a -> Gen (m a)) -> Gen a -> Gen (m a)) -> Gen a -> Gen (Blind (m a))
genM with = fmap Blind . go where
  go :: forall a . Gen a -> Gen (m a)
  go a = Gen.sized $ \case
    Size i
      | i <= 1 -> fmap pure a
      | otherwise -> Gen.choice [ fmap pure a, with (Gen.scale (`div` 2) . go) a]


genA :: Gen A
genA = Gen.integral (Range.linear 0 10)

newtype A = A { unA :: Integer }
  deriving (Enum, Eq, Generic, Integral, Num, Ord, Real, Show, Vary)

instance Arg A

genB :: Gen B
genB = Gen.integral (Range.linear 0 10)

newtype B = B { unB :: Integer }
  deriving (Enum, Eq, Generic, Integral, Num, Ord, Real, Show, Vary)

instance Arg B

genC :: Gen C
genC = Gen.integral (Range.linear 0 10)

newtype C = C { unC :: Integer }
  deriving (Enum, Eq, Generic, Integral, Num, Ord, Real, Show, Vary)

instance Arg C


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
