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
, With(labelWith, getWith)
, atom
, liftWith
, liftWith2
, liftWith2InfixL
, liftWith2InfixR
, addLabel
, labelling
  -- * Pattern synonyms
, pattern With
, pattern Fn
, pattern FnWith
  -- * Re-exports
, Gen
, (===)
, (/==)
, Gen.choice
, Arg
, Vary
, Gen.fn
, apply
) where

import Control.Carrier.Pure
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.Functor.Classes (showsUnaryWith)
import Data.Proxy
import qualified Data.Semigroup as S
import qualified Data.Set as Set
import Data.String (fromString)
import GHC.Stack
import GHC.TypeLits
import Hedgehog hiding (Gen)
import qualified Hedgehog
import Hedgehog.Function as Fn hiding (R, S)
import Hedgehog.Gen
import Hedgehog.Range

-- | A generator for computations, given a higher-order generator for effectful operations, & a generator for results.
m
  :: forall m a
  .  (Monad m, Show a)
  => (forall a . Show a => (forall a . Show a => Gen a -> Gen (m a)) -> Gen a -> Gen (m a)) -- ^ A higher-order generator producing operations using any effects in @m@.
  -> Gen a                                                                                        -- ^ A generator for results.
  -> Gen (m a)                                                                                    -- ^ A generator producing computations, wrapped in 'With' for convenience.
m with = go where
  go :: forall a . Show a => Gen a -> Gen (m a)
  go a = Gen $ recursive Hedgehog.Gen.choice
    [ runGen (addLabel "pure" (liftWith "pure" pure a)) ]
    [ frequency
      [ (3, runGen (with go a))
      , (1, runGen (addLabel ">>" (liftWith2InfixL 1 ">>" (>>) (go a) (go a))))
      ]
    ]


genT :: KnownSymbol s => Gen (T s)
genT = Gen (showing . T <$> integral (linear 0 100))

newtype T a = T { unT :: Integer }
  deriving (Enum, Eq, Generic, Num, Ord, Real, Vary)

instance Arg (T a)

instance S.Semigroup (T a) where
  T a <> T b = T (a + b)

instance Monoid (T a) where
  mempty = T 0
  mappend = (S.<>)

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
w = genT

type W = T "W"

fn :: (Arg a, Vary a, Show a) => Gen b -> Gen (a -> b)
fn = Gen . fmap (fmap (fmap getWith) . showingFn) . Fn.fn . runGen

choice :: [Gen a] -> Gen a
choice = Gen . Hedgehog.Gen.choice . Prelude.map runGen


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

instance (Forall (Rec gs) b) => Forall (Rec (Gen a ': gs)) (a -> b) where
  forall' (g :. gs) f = do
    a <- Hedgehog.forAll (runGen g)
    forall' gs (f (getWith a))


data With a = With' { labelWith :: Set.Set LabelName, _showWith :: Int -> ShowS, getWith :: a }
  deriving (Functor)

instance Eq a => Eq (With a) where
  (==) = (==) `on` getWith

instance Applicative With where
  pure = With' mempty (\ _ -> showString "_")
  With' lf sf f <*> With' la sa a = With' (mappend lf la) (\ d -> showParen (d > 10) (sf 10 . showString " " . sa 11)) (f a)

instance Show (With a) where
  showsPrec d (With' _ s _) = s d

showing :: Show a => a -> With a
showing = With' mempty . flip showsPrec <*> id

showingFn :: (Show a, Show b) => Fn a b -> With (a -> b)
showingFn = With' mempty . flip showsPrec <*> apply

atom :: String -> a -> Gen a
atom s = Gen . pure . With' mempty (\ _ -> showString s)

liftWith :: String -> (a -> b) -> Gen a -> Gen b
liftWith s w a = atom s w <*> a

liftWith2 :: String -> (a -> b -> c) -> Gen a -> Gen b -> Gen c
liftWith2 s w a b = atom s w <*> a <*> b

liftWith2InfixL :: Int -> String -> (a -> b -> c) -> Gen a -> Gen b -> Gen c
liftWith2InfixL p s f ga gb = Gen $ do
  With' la sa a <- runGen ga
  With' lb sb b <- runGen gb
  pure (With' (mappend la lb) (\ d -> showParen (d > p) (sa p . showString " " . showString s . showString " " . sb (succ p))) (f a b))

liftWith2InfixR :: Int -> String -> (a -> b -> c) -> Gen a -> Gen b -> Gen c
liftWith2InfixR p s f ga gb = Gen $ do
  With' la sa a <- runGen ga
  With' lb sb b <- runGen gb
  pure (With' (mappend la lb) (\ d -> showParen (d > p) (sa (succ p) . showString " " . showString s . showString " " . sb p)) (f a b))

addLabel :: String -> Gen a -> Gen a
addLabel s = Gen . fmap (\ w -> w { labelWith = Set.insert (fromString s) (labelWith w) }) . runGen

labelling :: (MonadTest m, HasCallStack) => With a -> m ()
labelling = withFrozenCallStack . traverse_ label . labelWith


pattern With :: a -> With a
pattern With a <- (With' _ _ a)

{-# COMPLETE With #-}

pattern Fn :: (a -> b) -> Fn a b
pattern Fn f <- (apply -> f)

{-# COMPLETE Fn #-}

pattern FnWith :: (a -> b) -> Fn a (With b)
pattern FnWith f <- (fmap getWith . apply -> f)

{-# COMPLETE FnWith #-}


newtype Gen a = Gen { runGen :: Hedgehog.Gen (With a) }
  deriving (Functor)

instance Applicative Gen where
  pure = Gen . pure . pure
  Gen m1 <*> Gen m2 = Gen ((<*>) <$> m1 <*> m2)
