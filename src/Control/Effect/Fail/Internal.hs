{-# LANGUAGE DeriveFunctor, PolyKinds #-}
module Control.Effect.Fail.Internal
( Fail(..)
) where

import Control.Effect.Handler
import Data.Coerce

newtype Fail m k = Fail String
  deriving (Functor)

instance HFunctor Fail where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect Fail where
  handle _ _ = coerce
