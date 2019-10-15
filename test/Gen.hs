{-# LANGUAGE DataKinds, DeriveFunctor, DeriveGeneric, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, PatternSynonyms, PolyKinds, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeOperators, UndecidableInstances, ViewPatterns #-}
module Gen
( module Control.Carrier.Pure
  -- * Polymorphic generation & instantiation
, m
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
  -- * Generation
, Rec(..)
, forall
  -- * Showing generated values
, With(getWith)
, showing
, showingFn
, atom
, liftWith
, liftWith2
  -- * Pattern synonyms
, pattern With
, pattern Fn
, pattern FnWith
  -- * Re-exports
, Gen
, (===)
, (/==)
, choice
, subterm
, subtermM
, subterm2
, Arg
, Vary
, fn
, apply
) where

import Control.Carrier.Pure
import Data.Function (on)
import Data.Functor.Classes (showsUnaryWith)
import Data.Proxy
import GHC.Stack
import GHC.TypeLits
import Hedgehog
import Hedgehog.Function hiding (R, S)
import Hedgehog.Gen
import Hedgehog.Range

-- | A generator for computations, given a higher-order generator for effectful operations, & a generator for results.
m
  :: forall m a
  .  (Applicative m, Show a)
  => (forall a . Show a => (forall a . Show a => Gen a -> Gen (With (m a))) -> Gen a -> Gen (With (m a))) -- ^ A higher-order generator producing operations using any effects in @m@.
  -> Gen a                                                                                      -- ^ A generator for results.
  -> Gen (With (m a))                                                                           -- ^ A generator producing computations, wrapped in 'With' for convenience.
m with = go where
  go :: forall a . Show a => Gen a -> Gen (With (m a))
  go a = recursive choice [(liftWith "pure" pure . showing) <$> a] [with go a]


genT :: Gen (T a)
genT = T <$> integral (linear 0 100)

newtype T a = T { unT :: Integer }
  deriving (Enum, Eq, Generic, Num, Ord, Real, Vary)

instance Arg (T a)

instance KnownSymbol s => Show (T s) where
  showsPrec d = showsUnaryWith showsPrec (symbolVal (Proxy @s)) d . unT

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

forall :: (Forall g f, HasCallStack) => g -> f -> Hedgehog.Property
forall g f = withFrozenCallStack $ Hedgehog.property (forall' g f)

class Forall g f | g -> f, f -> g where
  forall' :: HasCallStack => g -> f -> PropertyT IO ()

instance Forall (Rec '[]) (PropertyT IO ()) where
  forall' Nil = id

instance (Forall (Rec gs) b, Show a) => Forall (Rec (Gen a ': gs)) (a -> b) where
  forall' (g :. gs) f = do
    a <- Hedgehog.forAll g
    forall' gs (f a)


data With a = With' { _showWith :: Int -> ShowS, getWith :: a }
  deriving (Functor)

instance Eq a => Eq (With a) where
  (==) = (==) `on` getWith

instance Applicative With where
  pure = atom "_"
  With' sf f <*> With' sa a = With' (\ d -> showParen (d > 10) (sf 10 . showString " " . sa 11)) (f a)

instance Show (With a) where
  showsPrec d (With' s _) = s d

showing :: Show a => a -> With a
showing = With' . flip showsPrec <*> id

showingFn :: (Show a, Show b) => Fn a b -> With (a -> b)
showingFn = With' . flip showsPrec <*> apply

atom :: String -> a -> With a
atom s = With' (\ _ -> showString s)

liftWith :: String -> (a -> b) -> With a -> With b
liftWith s w a = atom s w <*> a

liftWith2 :: String -> (a -> b -> c) -> With a -> With b -> With c
liftWith2 s w a b = atom s w <*> a <*> b


pattern With :: a -> With a
pattern With a <- (getWith -> a)

{-# COMPLETE With #-}

pattern Fn :: (a -> b) -> Fn a b
pattern Fn f <- (apply -> f)

{-# COMPLETE Fn #-}

pattern FnWith :: (a -> b) -> Fn a (With b)
pattern FnWith f <- (fmap getWith . apply -> f)

{-# COMPLETE FnWith #-}
