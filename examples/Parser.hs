{-# LANGUAGE DeriveFunctor, FlexibleContexts, KindSignatures #-}
module Parser where

import Control.Effect.Carrier
import Control.Effect.Sum
import Data.Char
import Data.Coerce
import Test.Hspec

spec :: Spec
spec = describe "parser" $ pure ()

data Symbol (m :: * -> *) k = Satisfy (Char -> Bool) (Char -> k)
  deriving (Functor)

instance HFunctor Symbol where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect Symbol where
  handle state handler = coerce . fmap (handler . (<$ state))

satisfy :: (Carrier sig m, Member Symbol sig) => (Char -> Bool) -> m Char
satisfy p = send (Satisfy p ret)

char :: (Carrier sig m, Member Symbol sig) => Char -> m Char
char = satisfy . (==)

digit :: (Carrier sig m, Member Symbol sig) => m Char
digit = satisfy isDigit
