{-# LANGUAGE DeriveFunctor, KindSignatures #-}
module Parser where

import Control.Effect.Carrier
import Data.Coerce

data Symbol (m :: * -> *) k = Symbol Char (Char -> k)
  deriving (Functor)

instance HFunctor Symbol where
  hmap _ = coerce
  {-# INLINE hmap #-}
