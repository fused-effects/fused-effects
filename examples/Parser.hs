{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, KindSignatures, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Parser where

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.NonDet
import Control.Effect.Sum
import Data.Char
import Data.Coerce
import Data.List (intercalate)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = describe "parser" $ do
  describe "parse" $ do
    prop "returns pure values at the end of input" $
      \ a -> run (runNonDet (parse "" (pure a))) == [a :: Integer]

    prop "fails if input remains" $
      \ c cs a -> run (runNonDet (parse (c:cs) (pure (a :: Integer)))) == []

  describe "satisfy" $ do
    prop "matches with a predicate" $
      \ c f -> run (runNonDet (parse [c] (satisfy (applyFun f)))) == if applyFun f c then [c] else []

    prop "fails at end of input" $
      \ f -> run (runNonDet (parse "" (satisfy (applyFun f)))) == []

    prop "fails if input remains" $
      \ c1 c2 f -> run (runNonDet (parse [c1, c2] (satisfy (applyFun f)))) == []

    prop "consumes input" $
      \ c1 c2 f -> run (runNonDet (parse [c1, c2] ((,) <$> satisfy (applyFun f) <*> satisfy (applyFun f)))) == if applyFun f c1 && applyFun f c2 then [(c1, c2)] else []

  describe "factor" $ do
    prop "matches positive integers" $
      \ a -> run (runNonDet (parse (show (abs a)) factor)) == [abs a]

  describe "term" $ do
    prop "matches factors" $
      \ a -> run (runNonDet (parse (show (abs a)) term)) == [abs a]

    prop "matches multiplication" $
      \ as -> run (runNonDet (parse (intercalate "*" (show . abs <$> 1:as)) term)) == [product (map abs as)]

  describe "expr" $ do
    prop "matches factors" $
      \ a -> run (runNonDet (parse (show (abs a)) expr)) == [abs a]

    prop "matches multiplication" $
      \ as -> run (runNonDet (parse (intercalate "*" (show . abs <$> 1:as)) expr)) == [product (map abs as)]


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
