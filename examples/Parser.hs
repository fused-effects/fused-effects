{-# LANGUAGE DeriveFunctor, KindSignatures #-}
module Parser where

import Control.Effect.Carrier
import Data.Coerce
import Test.Hspec

spec :: Spec
spec = describe "parser" $ pure ()

data Symbol (m :: * -> *) k = Symbol Char (Char -> k)
  deriving (Functor)

instance HFunctor Symbol where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect Symbol where
  handle state handler = coerce . fmap (handler . (<$ state))
