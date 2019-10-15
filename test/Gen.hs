{-# LANGUAGE DataKinds, DeriveFunctor, DeriveGeneric, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, PolyKinds, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeOperators, UndecidableInstances, ViewPatterns #-}
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
, atom
, Gen.label
, infixL
, infixR
, liftWith
, liftWith2
, liftWith2InfixL
, liftWith2InfixR
, addLabel
  -- * Re-exports
, Gen
, (===)
, (/==)
, Gen.choice
, Fn.Arg
, Fn.Vary
, Gen.fn
, Fn.apply
, Term(..)
) where

import Control.Applicative
import Control.Carrier.Pure
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
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
import qualified Hedgehog.Function as Fn
import Hedgehog.Gen
import Hedgehog.Range

-- | A generator for computations, given a higher-order generator for effectful operations, & a generator for results.
m
  :: forall m a
  .  (Monad m, Show a)
  => (forall a . Show a => (forall a . Show a => Gen a -> Gen (m a)) -> Gen a -> Gen (m a)) -- ^ A higher-order generator producing operations using any effects in @m@.
  -> Gen a                                                                                  -- ^ A generator for results.
  -> Gen (m a)                                                                              -- ^ A generator producing computations.
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
  deriving (Enum, Eq, Fn.Generic, Num, Ord, Real, Fn.Vary)

instance Fn.Arg (T a)

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

fn :: (Fn.Arg a, Fn.Vary a, Show a) => Gen b -> Gen (a -> b)
fn b = Gen (lift (fmap (fmap getWith) . showingFn <$> Fn.fn (fst <$> runWriterT (runGen b))))

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
    (a, labels) <- Hedgehog.forAll (runWriterT (runGen g))
    traverse_ Hedgehog.label labels
    forall' gs (f (getWith a))


data With a = With { showWith :: Term a, getWith :: a }
  deriving (Functor)

instance Eq a => Eq (With a) where
  (==) = (==) `on` getWith

instance Applicative With where
  pure = With . pure <*> id
  With sf f <*> With sa a = With (sf <*> sa) (f a)

instance Show (With a) where
  showsPrec d = showsPrec d . showWith

showing :: Show a => a -> With a
showing = With . Pure . flip showsPrec <*> id

showingFn :: (Show a, Show b) => Fn.Fn a b -> With (a -> b)
showingFn = With . Pure . flip showsPrec <*> Fn.apply

atom :: String -> a -> Gen a
atom s = Gen . pure . With (Pure (const (showString s)))

label :: String -> a -> Gen a
label s = addLabel s . atom s

infixL :: Int -> String -> (a -> b -> c) -> Gen (a -> b -> c)
infixL p s f = Gen (pure (With (InfixL p s) f))

infixR :: Int -> String -> (a -> b -> c) -> Gen (a -> b -> c)
infixR p s f = Gen (pure (With (InfixR p s) f))

liftWith :: String -> (a -> b) -> Gen a -> Gen b
liftWith s w a = atom s w <*> a

liftWith2 :: String -> (a -> b -> c) -> Gen a -> Gen b -> Gen c
liftWith2 s w a b = atom s w <*> a <*> b

liftWith2InfixL :: Int -> String -> (a -> b -> c) -> Gen a -> Gen b -> Gen c
liftWith2InfixL p s f ga gb = infixL p s f <*> ga <*> gb

liftWith2InfixR :: Int -> String -> (a -> b -> c) -> Gen a -> Gen b -> Gen c
liftWith2InfixR p s f ga gb = infixR p s f <*> ga <*> gb

addLabel :: String -> Gen a -> Gen a
addLabel s = Gen . (>>= \ a -> a <$ tell (Set.singleton (fromString s))) . runGen


data Term a where
  Pure :: (Int -> ShowS) -> Term a
  InfixL :: Int -> String -> Term (a -> b -> c)
  InfixR :: Int -> String -> Term (a -> b -> c)
  (:<*>) :: Term (a -> b) -> Term a -> Term b

infixl 4 :<*>

instance Functor Term where
  fmap = liftA

instance Applicative Term where
  pure _ = Pure (const (showString "_"))
  (<*>) = (:<*>)

instance Show (Term a) where
  showsPrec d = \case
    Pure s -> s d
    InfixL _ s -> showParen True (showString s)
    InfixR _ s -> showParen True (showString s)
    InfixL p s :<*> a :<*> b -> showParen (d > p) (showsPrec p a . showString " " . showString s . showString " " . showsPrec (succ p) b)
    InfixR p s :<*> a :<*> b -> showParen (d > p) (showsPrec (succ p) a . showString " " . showString s . showString " " . showsPrec p b)
    InfixL p s :<*> a -> showParen True (showsPrec p a . showString " " . showString s)
    InfixR p s :<*> a -> showParen True (showsPrec (succ p) a . showString " " . showString s)
    f :<*> a -> showParen (d > 10) (showsPrec 10 f . showString " " . showsPrec 11 a)


newtype Gen a = Gen { runGen :: WriterT (Set.Set LabelName) Hedgehog.Gen (With a) }
  deriving (Functor)

instance Applicative Gen where
  pure = Gen . pure . pure
  Gen m1 <*> Gen m2 = Gen ((<*>) <$> m1 <*> m2)
