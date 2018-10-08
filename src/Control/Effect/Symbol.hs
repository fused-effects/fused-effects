{-# LANGUAGE DeriveFunctor, FlexibleContexts, PolyKinds, TypeOperators #-}
module Control.Effect.Symbol where

import Control.Applicative (Alternative(..))
import Control.Effect
import Control.Effect.Cut
import Control.Effect.State

data Symbol m k
  = Symbol (Char -> Bool) (Char -> k)
  deriving (Functor)

instance Effect Symbol where
  hfmap _ (Symbol sat k) = Symbol sat k

  handle _ (Symbol sat k) = Symbol sat k

satisfy :: Subset Symbol sig => (Char -> Bool) -> Eff sig Char
satisfy sat = send (Symbol sat pure)

char :: Subset Symbol sig => Char -> Eff sig Char
char c = satisfy (== c)

digit :: (Subset NonDet sig, Subset Symbol sig) => Eff sig Char
digit = foldr ((<|>) . char) empty ['0'..'9']

expr :: (Subset Cut sig, Subset NonDet sig, Subset Symbol sig) => Eff sig Int
expr = do
  i <- term
  call ((i +) <$ char '+' <* cut <*> expr) <|> pure i

term :: (Subset Cut sig, Subset NonDet sig, Subset Symbol sig) => Eff sig Int
term = do
  i <- factor
  call ((i *) <$ char '*' <* cut <*> term) <|> pure i

factor :: (Subset Cut sig, Subset NonDet sig, Subset Symbol sig) => Eff sig Int
factor = read <$> some digit
     <|> char '(' *> expr <* char ')'

parse :: Subset NonDet sig => String -> Eff (Symbol :+: sig) a -> Eff sig a
parse input = fmap snd . flip runStateH input . relay alg
  where alg (Symbol p k) = StateH (\ s -> case s of
          c:cs | p c -> runStateH (k c) cs
          _          -> empty)
