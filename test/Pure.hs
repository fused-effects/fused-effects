{-# LANGUAGE DataKinds, DeriveFunctor, DeriveGeneric, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, PolyKinds, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Pure
( module Control.Carrier.Pure
, (~=)
, genM
, genT
, a
, b
, e
, r
, s
, w
, T(..)
, A
, B
, E
, R
, S
, W
, Rec(..)
, forall
, With(..)
, showing
, showingFn
) where

import Control.Carrier.Pure
import Hedgehog
import Hedgehog.Function hiding (R, S)
import Hedgehog.Gen
import Hedgehog.Range

(~=) :: (Eq a, Show a) => PureC a -> PureC a -> PropertyT IO ()
m1 ~= m2 = run m1 === run m2


-- | A generator for computations, given a higher-order generator for effectful operations, & a generator for results.
genM
  :: forall m a
  .  Applicative m
  => (forall a . (forall a . Gen a -> Gen (m a)) -> Gen a -> Gen (m a)) -- ^ A higher-order generator producing operations using any effects in @m@.
  -> Gen a                                                              -- ^ A generator for results.
  -> Gen (With (m a))                                                   -- ^ A generator producing computations, wrapped in 'With' for convenience.
genM with = fmap pure . go where
  go :: forall a . Gen a -> Gen (m a)
  go a = recursive choice [fmap pure a] [with go a]


genT :: Gen (T a)
genT = T <$> integral (linear 0 100)

newtype T a = T { unT :: Integer }
  deriving (Enum, Eq, Generic, Num, Ord, Real, Show, Vary)

instance Arg (T a)

a :: Gen A
a = genT

type A = T "A"

b :: Gen B
b = genT

type B = T "B"

e :: Gen E
e = genT

type E = T "E"

r :: Gen R
r = genT

type R = T "R"

s :: Gen S
s = genT

type S = T "S"

w :: Gen W
w = list (linear 0 100) genT

type W = [T "W"]


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


data With a = With { showWith :: String, getWith :: a }
  deriving (Eq, Functor, Ord)

instance Applicative With where
  pure = With "_"
  With sf f <*> With sa a = With (sf <> " " <> parens sa) (f a) where
    parens s = "(" <> s <> ")"

instance Show (With a) where
  show = showWith

showing :: Show a => a -> With a
showing = With . show <*> id

showingFn :: (Show a, Show b) => Fn a b -> With (a -> b)
showingFn = With . show <*> apply
