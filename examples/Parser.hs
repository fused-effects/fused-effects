{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, KindSignatures, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Parser where

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.NonDet
import Control.Effect.Sum
import Data.Char
import Data.Coerce
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = describe "parser" $ do
  describe "satisfy" $ do
    prop "matches with a predicate" $
      \ c f -> run (runNonDetOnce (parse [c] (satisfy (applyFun f)))) == if applyFun f c then Just c else Nothing

    prop "fails at end of input" $
      \ f -> run (runNonDetOnce (parse "" (satisfy (applyFun f)))) == Nothing


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

parens :: (Applicative m, Carrier sig m, Member Symbol sig) => m a -> m a
parens m = char '(' *> m <* char ')'


parse :: (Alternative m, Carrier sig m, Effect sig, Monad m) => String -> Eff (ParseC m) a -> m a
parse input = (>>= exhaustive) . flip runParseC input . interpret
  where exhaustive ("", a) = pure a
        exhaustive _       = empty

newtype ParseC m a = ParseC { runParseC :: String -> m (String, a) }

instance (Alternative m, Carrier sig m, Effect sig) => Carrier (Symbol :+: sig) (ParseC m) where
  ret a = ParseC (\ input -> ret (input, a))
  eff op = ParseC (\ input -> handleSum
    (eff . handleState input runParseC)
    (\ (Satisfy p k) -> case input of
      c:cs | p c -> runParseC (k c) cs
      _          -> empty)
    op)


expr :: (Alternative m, Carrier sig m, Member Symbol sig) => m Int
expr
  =   (+) <$> term <* char '+' <*> expr
  <|> term

term :: (Alternative m, Carrier sig m, Member Symbol sig) => m Int
term
  =   (*) <$> factor <* char '*' <*> term
  <|> factor

factor :: (Alternative m, Carrier sig m, Member Symbol sig) => m Int
factor = read <$> some digit
