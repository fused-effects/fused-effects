{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Control.Effect.Symbol where

import Control.Applicative (Alternative(..))
import Control.Effect
import Control.Effect.Cut

data Symbol m k
  = Symbol (Char -> Bool) (Char -> k)
  deriving (Functor)

instance Effect Symbol where
  hfmap _ (Symbol sat k) = Symbol sat k

  handle _ (Symbol sat k) = Symbol sat k

satisfy :: (Subset Symbol sig, TermMonad m sig) => (Char -> Bool) -> m Char
satisfy sat = send (Symbol sat pure)

char :: (Subset Symbol sig, TermMonad m sig) => Char -> m Char
char c = satisfy (== c)

digit :: (Alternative m, Subset Symbol sig, TermMonad m sig) => m Char
digit = foldr ((<|>) . char) empty ['0'..'9']

expr :: (Alternative m, Subset Cut sig, Subset Symbol sig, TermMonad m sig) => m Int
expr = do
  i <- term
  call ((i +) <$ char '+' <* cut <*> expr) <|> pure i

term :: (Alternative m, Subset Cut sig, Subset Symbol sig, TermMonad m sig) => m Int
term = do
  i <- factor
  call ((i *) <$ char '*' <* cut <*> term) <|> pure i

factor :: (Alternative m, Subset Cut sig, Subset Symbol sig, TermMonad m sig) => m Int
factor = read <$> some digit
     <|> char '(' *> expr <* char ')'


parse :: (Alternative m, TermMonad m sig) => String -> Eff (SymbolH m) a -> m a
parse input = fmap snd . flip runSymbolH input . runEff var

newtype SymbolH m a = SymbolH { runSymbolH :: String -> m (String, a) }

instance Carrier ((,) String) SymbolH where
  joinl mf = SymbolH (\ s -> mf >>= \ f -> runSymbolH f s)

  suspend f = SymbolH (\ s -> runSymbolH (f (s, ())) s)

  resume (s, m) = runSymbolH m s

  wrap = SymbolH . const

instance (Alternative m, TermMonad m sig) => TermAlgebra (SymbolH m) (Symbol :+: sig) where
  var a = SymbolH (\ s -> pure (s, a))
  con = alg \/ algRest
    where alg (Symbol p k) = SymbolH (\ s -> case s of
            c:cs | p c -> runSymbolH (k c) cs
            _          -> empty)
