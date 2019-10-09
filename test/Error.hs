{-# LANGUAGE FlexibleContexts #-}
module Error
( throwError_annihilation
, catchError_substitution
) where

import Control.Carrier
import Control.Effect.Error

throwError_annihilation :: Has (Error e) sig m => (m b -> m b -> prop) -> e -> (a -> m b) -> prop
throwError_annihilation (===) e k = (throwError e >>= k) === throwError e

catchError_substitution :: Has (Error e) sig m => (m a -> m a -> prop) -> e -> (e -> m a) -> prop
catchError_substitution (===) e f = (throwError e `catchError` f) === f e
