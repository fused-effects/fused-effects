{-# LANGUAGE DeriveGeneric, FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Error
( throwError_annihilation
, catchError_substitution
, E
, A
, B
) where

import Control.Carrier
import Control.Effect.Error
import GHC.Generics (Generic)
import Test.Tasty.QuickCheck

throwError_annihilation :: Has (Error e) sig m => (m b -> m b -> prop) -> e -> (a -> m b) -> prop
throwError_annihilation (===) e k = (throwError e >>= k) === throwError e

catchError_substitution :: Has (Error e) sig m => (m a -> m a -> prop) -> e -> (e -> m a) -> prop
catchError_substitution (===) e f = (throwError e `catchError` f) === f e


newtype E = E Integer
  deriving (Arbitrary, CoArbitrary, Eq, Generic, Ord, Show)

instance Function    E

newtype A = A Integer
  deriving (Arbitrary, CoArbitrary, Eq, Generic, Ord, Show)

instance Function    A

newtype B = B Integer
  deriving (Arbitrary, CoArbitrary, Eq, Generic, Ord, Show)

instance Function    B
