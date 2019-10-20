{-# LANGUAGE DataKinds, DeriveFunctor, DeriveGeneric, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, PatternSynonyms, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeOperators, UndecidableInstances, ViewPatterns #-}
{-# OPTIONS_GHC -Wno-identities #-}
module Gen
( module Control.Carrier.Pure
  -- * Polymorphic generation & instantiation
, m
, GenM
, genT
, T(..)
, a
, A
, b
, B
, c
, C
, e
, E
, r
, R
, s
, S
, w
, W
, unit
, identity
  -- * Handlers
, Run(..)
, type RunL
, pattern RunL
, type RunR
, pattern RunR
, type RunC
, pattern RunC
  -- * Generation
, Rec(..)
, forall
  -- * Showing generated values
, atom
, Gen.label
, infixL
, infixR
, addLabel
  -- * Re-exports
, Gen
, (===)
, (/==)
, Gen.choice
, Gen.integral
, Gen.unicode
, Gen.string
, Fn.Arg
, Fn.Vary
, Gen.fn
, termFn
, Fn.apply
) where

import Control.Applicative
import Control.Carrier.Pure
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.Foldable (traverse_)
import Data.Functor.Classes (showsUnaryWith)
import Data.Functor.Identity
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
  :: forall m
  .  Monad m
  => (GenM m -> GenM m) -- ^ A higher-order computation generator using any effects in @m@.
  -> GenM m             -- ^ A computation generator.
m with = m where
  m :: GenM m
  m a = Gen $ scale (`div` 2) $ recursive Hedgehog.Gen.choice
    [ runGen (Gen.label "pure" pure <*> a) ]
    [ frequency
      [ (3, runGen (with m a))
      , (1, runGen (addLabel ">>" (infixL 1 ">>" (>>) <*> m a <*> m a)))
      ]
    ]

-- | Computation generators are higher-order generators of computations in some monad @m@.
type GenM m = (forall a . Gen a -> Gen (m a))


genT :: KnownSymbol s => Gen (T s)
genT = Gen.integral (linear 0 100)

newtype T (a :: Symbol) = T { unT :: Integer }
  deriving (Enum, Eq, Fn.Generic, Integral, Num, Ord, Real, Fn.Vary)

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

c :: Gen C
c = genT

type C = T "C"

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

unit :: Gen ()
unit = atom "()" ()

identity :: Gen (a -> Identity a)
identity = atom "Identity" Identity

fn :: (Fn.Arg a, Fn.Vary a, Show a) => Gen b -> Gen (a -> b)
fn b = Gen (lift (fmap (fmap runTerm) . showingFn <$> Fn.fn (fst <$> runWriterT (runGen b))))

termFn :: Gen b -> Gen (a -> b)
termFn b = Gen $ recursive Hedgehog.Gen.choice
  [ runGen (atom "const" const <*> b) ]
  []

choice :: [Gen a] -> Gen a
choice = Gen . Hedgehog.Gen.choice . Prelude.map runGen

integral :: (Integral a, Show a) => Range a -> Gen a
integral range = Gen (showing <$> Hedgehog.Gen.integral range)

unicode :: Gen Char
unicode = Gen (showing <$> Hedgehog.Gen.unicode)

string :: Range Int -> Gen Char -> Gen String
string range cs = Gen (showing <$> Hedgehog.Gen.string range (runTerm <$> runGen cs))


-- | This captures the shape of the handler function passed to the "Monad" & "MonadFix" tests.
newtype Run f g m = Run (forall a . f (m a) -> PureC (g a))

-- | The type of handlers with output state, but no input state (e.g. 'Control.Carrier.Error.Either.ErrorC').
type RunL g m = Run Identity g m

-- | Handlers with output state, but no input state (e.g. 'Control.Carrier.Error.Either.ErrorC').
pattern RunL :: (forall a . m a -> PureC (f a)) -> Run Identity f m
pattern RunL run <- Run ((.# Identity) -> run) where
  RunL run = Run (run . runIdentity)

{-# COMPLETE RunL #-}

-- | The type of handlers with input state, but no output state (e.g. 'Control.Carrier.Reader.ReaderC').
type RunR f m = Run f Identity m

-- | Handlers with input state, but no output state (e.g. 'Control.Carrier.Reader.ReaderC').
pattern RunR :: (forall a . f (m a) -> PureC a) -> Run f Identity m
pattern RunR run <- Run ((fmap runIdentity #.) -> run) where
  RunR run = Run (fmap Identity . run)

{-# COMPLETE RunR #-}

-- | The type of handlers with curried input state (e.g. 'Control.Carrier.Reader.ReaderC', 'Control.Carrier.State.Strict.StateC').
type RunC s f m = Run ((,) s) f m

-- | Handlers with curried input state (e.g. 'Control.Carrier.Reader.ReaderC', 'Control.Carrier.State.Strict.StateC').
pattern RunC :: (forall a . s -> m a -> PureC (f a)) -> Run ((,) s) f m
pattern RunC run <- Run (curry' -> run) where
  RunC run = Run (uncurry run)

{-# COMPLETE RunC #-}


-- Regrettable necessities for composing rank-n functions.

(.#) :: (forall a . f (m a) -> PureC (g a)) -> (forall a . m a -> f (m a)) -> (forall a . m a -> PureC (g a))
(f .# g) m = f (g m)

(#.) :: (forall a . PureC (g a) -> PureC a) -> (forall a . f (m a) -> PureC (g a)) -> (forall a . f (m a) -> PureC a)
(f #. g) m = f (g m)

curry' :: (forall a . (s, m a) -> PureC (g a)) -> (forall a . s -> m a -> PureC (g a))
curry' f = \ s m -> f (s, m)


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
    HideLabels (a, labels) <- Hedgehog.forAll (HideLabels <$> runWriterT (runGen g))
    traverse_ Hedgehog.label labels
    forall' gs (f (runTerm a))

newtype HideLabels a = HideLabels { unHideLabels :: (a, Set.Set LabelName) }

instance Show a => Show (HideLabels a) where
  showsPrec d = showsPrec d . fst . unHideLabels


showing :: Show a => a -> Term a
showing = Pure . flip showsPrec <*> id

showingFn :: (Show a, Show b) => Fn.Fn a b -> Term (a -> b)
showingFn = Pure . flip showsPrec <*> Fn.apply

atom :: String -> a -> Gen a
atom s = Gen . pure . Pure (const (showString s))

label :: String -> a -> Gen a
label s = addLabel s . atom s

infixL :: Int -> String -> (a -> b -> c) -> Gen (a -> b -> c)
infixL p s f = Gen (pure (InfixL p s f))

infixR :: Int -> String -> (a -> b -> c) -> Gen (a -> b -> c)
infixR p s f = Gen (pure (InfixR p s f))

addLabel :: String -> Gen a -> Gen a
addLabel s = Gen . (>>= \ a -> a <$ tell (Set.singleton (fromString s))) . runGen


data Term a where
  Pure :: (Int -> ShowS) -> a -> Term a
  InfixL :: Int -> String -> (a -> b -> c) -> Term (a -> b -> c)
  InfixR :: Int -> String -> (a -> b -> c) -> Term (a -> b -> c)
  (:<*>) :: Term (a -> b) -> Term a -> Term b

infixl 4 :<*>

runTerm :: Term a -> a
runTerm = \case
  Pure _ a -> a
  InfixL _ _ f -> f
  InfixR _ _ f -> f
  f :<*> a -> runTerm f $ runTerm a

instance Functor Term where
  fmap = liftA

instance Applicative Term where
  pure = Pure (const (showString "_"))
  (<*>) = (:<*>)

instance Show (Term a) where
  showsPrec d = \case
    Pure s _ -> s d
    InfixL _ s _ -> showParen True (showString s)
    InfixR _ s _ -> showParen True (showString s)
    InfixL p s _ :<*> a :<*> b -> showParen (d > p) (showsPrec p a . showString " " . showString s . showString " " . showsPrec (succ p) b)
    InfixR p s _ :<*> a :<*> b -> showParen (d > p) (showsPrec (succ p) a . showString " " . showString s . showString " " . showsPrec p b)
    InfixL p s _ :<*> a -> showParen True (showsPrec p a . showString " " . showString s)
    InfixR p s _ :<*> a -> showParen True (showsPrec (succ p) a . showString " " . showString s)
    f :<*> a -> showParen (d > 10) (showsPrec 10 f . showString " " . showsPrec 11 a)


newtype Gen a = Gen { runGen :: WriterT (Set.Set LabelName) Hedgehog.Gen (Term a) }
  deriving (Functor)

instance Applicative Gen where
  pure = Gen . pure . pure
  Gen m1 <*> Gen m2 = Gen ((<*>) <$> m1 <*> m2)
