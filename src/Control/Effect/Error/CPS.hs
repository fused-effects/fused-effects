{-# LANGUAGE DeriveFunctor, RankNTypes #-}
module Control.Effect.Error.CPS
( -- * Error effect
  module Control.Effect.Error
  -- * Error carrier
, ErrorC(..)
) where

import Control.Effect.Error (Error, throwError, catchError)

newtype ErrorC e m a = ErrorC { runErrorC :: forall b . (e -> m b) -> (a -> m b) -> m b }
  deriving (Functor)
