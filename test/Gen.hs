{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-identities #-}
module Gen
( module Data.Functor.Identity
  -- * Polymorphic generation & instantiation
, genM
, GenM
, genT
, T(..)
, termA
, A
, termB
, B
, termC
, C
, termE
, E
, termR
, R
, termS
, S
, termW
, W
, unit
, identity
  -- * Handlers
, Run(..)
, runL
, runR
, runC
  -- * Generation
, Rec(..)
, forall_
  -- * Showing generated values
, showing
, GenTerm
, atom
, Gen.label
, infixL
, infixR
, pair
, addLabel
  -- * Test trees
, TestTree
, checkTestTree
, testGroup
, testProperty
  -- * Re-exports
, Gen
, (===)
, (/==)
, Gen.choice
, Gen.integral
, Gen.unicode
, Gen.string
, Gen.subtermM
, Gen.subtermM2
, Fn.Arg
, Fn.Vary
, Gen.fn
, termFn
, Fn.apply
) where

import           Control.Applicative
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer
import           Data.Foldable (traverse_)
import           Data.Function (on)
import           Data.Functor.Classes (showsUnaryWith)
import           Data.Functor.Identity
import           Data.Proxy
import qualified Data.Semigroup as S
import qualified Data.Set as Set
import           Data.String (fromString)
import           GHC.Generics ((:.:)(..))
import           GHC.Stack
import           GHC.TypeLits
import           Hedgehog hiding (Rec (..))
import qualified Hedgehog.Function as Fn
import           Hedgehog.Gen as Hedgehog
import           Hedgehog.Range

-- | A generator for computations, given a higher-order generator for effectful operations, & a generator for results.
genM
  :: forall m
  .  Monad m
  => (forall a . GenTerm a -> [GenTerm (m a)])
  -> (forall a . GenM m -> GenTerm a -> [GenTerm (m a)]) -- ^ A higher-order computation generator using any effects in @m@.
  -> GenM m                                              -- ^ A computation generator.
genM terminals nonterminals = m where
  m :: GenM m
  m a = Comp1 $ scale (`div` 2) $ recursive Hedgehog.choice
    (unComp1 <$> ((Gen.label "pure" pure <*> a) : terminals a))
    ( unComp1 (addLabel ">>" (Gen.subtermM2 (m a) (m a) (\ a b -> infixL 1 ">>" (>>) <*> a <*> b)))
    : (unComp1 <$> nonterminals m a))

-- | Computation generators are higher-order generators of computations in some monad @m@.
type GenM m = (forall a . GenTerm a -> GenTerm (m a))


genT :: KnownSymbol s => GenTerm (T s)
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

termA :: GenTerm A
termA = genT

type A = T "A"

termB :: GenTerm B
termB = genT

type B = T "B"

termC :: GenTerm C
termC = genT

type C = T "C"

termE :: GenTerm E
termE = genT

type E = T "E"

termR :: GenTerm R
termR = genT

type R = T "R"

termS :: GenTerm S
termS = genT

type S = T "S"

termW :: GenTerm W
termW = genT

type W = T "W"

unit :: GenTerm ()
unit = atom "()" ()

identity :: GenTerm (a -> Identity a)
identity = atom "Identity" Identity

fn :: (Fn.Arg a, Fn.Vary a, Show a) => GenTerm b -> GenTerm (a -> b)
fn b = Comp1 (lift (fmap (fmap runTerm) . showingFn <$> Fn.fn (fst <$> runWriterT (unComp1 b))))

termFn :: GenTerm b -> GenTerm (a -> b)
termFn b = Comp1 $ recursive Hedgehog.choice
  [ unComp1 (atom "const" const <*> b) ]
  []

choice :: [GenTerm a] -> GenTerm a
choice = Comp1 . Hedgehog.choice . Prelude.map unComp1

integral :: (Integral a, Show a) => Range a -> GenTerm a
integral range = Comp1 (showing <$> Hedgehog.integral range)

unicode :: GenTerm Char
unicode = Comp1 (showing <$> Hedgehog.unicode)

string :: Range Int -> GenTerm Char -> GenTerm String
string range cs = Comp1 (showing <$> Hedgehog.string range (runTerm <$> unComp1 cs))

subtermM :: GenTerm a -> (GenTerm a -> GenTerm a) -> GenTerm a
subtermM t f = Comp1 (Hedgehog.subtermM (unComp1 t) (unComp1 . f . term))

subtermM2 :: GenTerm a -> GenTerm a -> (GenTerm a -> GenTerm a -> GenTerm a) -> GenTerm a
subtermM2 t1 t2 f = Comp1 (Hedgehog.subtermM2 (unComp1 t1) (unComp1 t2) (fmap unComp1 . f `on` term))


-- | This captures the shape of the handler function passed to the "Monad" & "MonadFix" tests.
newtype Run f g m = Run (forall a . f (m a) -> Identity (g a))

-- | Handlers with output state, but no input state (e.g. 'Control.Carrier.Error.Either.ErrorC').
runL :: (forall a . m a -> Identity (f a)) -> Run Identity f m
runL run = Run (run . runIdentity)

-- | Handlers with input state, but no output state (e.g. 'Control.Carrier.Reader.ReaderC').
runR :: (forall a . f (m a) -> Identity a) -> Run f Identity m
runR run = Run (fmap Identity . run)

-- | Handlers with curried input state (e.g. 'Control.Carrier.Reader.ReaderC', 'Control.Carrier.State.Strict.StateC').
runC :: (forall a . s -> m a -> Identity (f a)) -> Run ((,) s) f m
runC run = Run (uncurry run)


infixr 5 :.

data Rec as where
  Nil :: Rec '[]
  (:.) :: a -> Rec as -> Rec (a ': as)

forall_ :: (Forall g f, HasCallStack) => g -> f -> Hedgehog.Property
forall_ g f = withFrozenCallStack $ Hedgehog.property (forall' g f)

class Forall g f | g -> f, f -> g where
  forall' :: HasCallStack => g -> f -> PropertyT IO ()

instance Forall (Rec '[]) (PropertyT IO ()) where
  forall' Nil = id

instance (Forall (Rec gs) b) => Forall (Rec (GenTerm a ': gs)) (a -> b) where
  forall' (g :. gs) f = do
    HideLabels (a, labels) <- Hedgehog.forAll (HideLabels <$> runWriterT (unComp1 g))
    traverse_ Hedgehog.label labels
    forall' gs (f (runTerm a))

newtype HideLabels a = HideLabels { unHideLabels :: (a, Set.Set LabelName) }

instance Show a => Show (HideLabels a) where
  showsPrec d = showsPrec d . fst . unHideLabels


showing :: Show a => a -> Term a
showing = Pure . flip showsPrec <*> id

showingFn :: (Show a, Show b) => Fn.Fn a b -> Term (a -> b)
showingFn = Pure . flip showsPrec <*> Fn.apply


type GenTerm = WriterT (Set.Set LabelName) Gen :.: Term

term :: Term a -> GenTerm a
term = Comp1 . pure

atom :: String -> a -> GenTerm a
atom s = term . Pure (const (showString s))

label :: String -> a -> GenTerm a
label s = addLabel s . atom s

infixL :: Int -> String -> (a -> b -> c) -> GenTerm (a -> b -> c)
infixL p s f = term (InfixL p s f)

infixR :: Int -> String -> (a -> b -> c) -> GenTerm (a -> b -> c)
infixR p s f = term (InfixR p s f)

pair :: GenTerm (a -> b -> (a, b))
pair = term Pair

addLabel :: String -> GenTerm a -> GenTerm a
addLabel s = Comp1 . (>>= (<$ tell (Set.singleton (fromString s)))) . unComp1


data Term a where
  Pure :: (Int -> ShowS) -> a -> Term a
  InfixL :: Int -> String -> (a -> b -> c) -> Term (a -> b -> c)
  InfixR :: Int -> String -> (a -> b -> c) -> Term (a -> b -> c)
  Pair :: Term (a -> b -> (a, b))
  (:<*>) :: Term (a -> b) -> Term a -> Term b

infixl 4 :<*>

runTerm :: Term a -> a
runTerm = \case
  Pure _ a -> a
  InfixL _ _ f -> f
  InfixR _ _ f -> f
  Pair -> (,)
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
    Pair -> showParen True (showString ",")
    InfixL p s _ :<*> a :<*> b -> showParen (d > p) (showsPrec p a . showString " " . showString s . showString " " . showsPrec (succ p) b)
    InfixR p s _ :<*> a :<*> b -> showParen (d > p) (showsPrec (succ p) a . showString " " . showString s . showString " " . showsPrec p b)
    Pair :<*> a :<*> b -> showParen True (shows a . showString ", " . shows b)
    InfixL p s _ :<*> a -> showParen True (showsPrec p a . showString " " . showString s)
    InfixR p s _ :<*> a -> showParen True (showsPrec (succ p) a . showString " " . showString s)
    f :<*> a -> showParen (d > 10) (showsPrec 10 f . showString " " . showsPrec 11 a)


data TestTree
  = Leaf String Property
  | Branch String [TestTree]

checkTestTree :: TestTree -> IO Bool
checkTestTree = \case
  Leaf   n p  ->        putStrLn n  *> check p                   <* putStrLn ""
  Branch n ts -> and <$ putStrLn n <*> traverse checkTestTree ts <* putStrLn ""

testGroup :: String -> [TestTree] -> TestTree
testGroup = Branch

testProperty :: String -> Property -> TestTree
testProperty = Leaf
