{-# LANGUAGE FlexibleContexts #-}
module Error
( throwError_annihilation
, catchError_substitution
) where

import Control.Carrier
import Control.Effect.Error
import Test.Tasty.QuickCheck

throwError_annihilation :: Has (Error e) sig m => (m b -> m b -> prop) -> e -> (a -> m b) -> prop
throwError_annihilation (===) e k = (throwError e >>= k) === throwError e

catchError_substitution :: (Eq (m a), Has (Error e) sig m, Show (m a)) => e -> (e -> m a) -> Property
catchError_substitution e f = (throwError e `catchError` f) === f e
