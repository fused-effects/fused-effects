{-# LANGUAGE FlexibleContexts #-}
module Error
( throwError_annihilation
, catchError_substitution
) where

import Control.Carrier
import Control.Effect.Error
import Test.QuickCheck

throwError_annihilation :: (Eq (m b), Has (Error e) sig m, Show (m b)) => e -> (a -> m b) -> Property
throwError_annihilation e f = (throwError e >>= f) === throwError e

catchError_substitution :: (Eq (m a), Has (Error e) sig m, Show (m a)) => e -> (e -> m a) -> Property
catchError_substitution e f = (throwError e `catchError` f) === f e
