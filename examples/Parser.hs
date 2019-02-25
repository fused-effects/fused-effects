{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, ExistentialQuantification, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Parser
( spec
) where

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Cut
import Control.Effect.NonDet
import Control.Effect.Sum hiding (L)
import Control.Monad (replicateM)
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
      \ a -> run (runNonDet (runCut (parse (show (abs a)) factor))) == [abs a]

    prop "matches parenthesized expressions" . forAll (sized arbNested) $
      \ as -> run (runNonDet (runCut (parse ('(' : intercalate "+" (intercalate "*" . map (show . abs) . (1:) <$> [0]:as) ++ ")") factor))) == [sum (map (product . map abs) as)]

  describe "term" $ do
    prop "matches factors" $
      \ a -> run (runNonDet (runCut (parse (show (abs a)) term))) == [abs a]

    prop "matches multiplication" $
      \ as -> run (runNonDet (runCut (parse (intercalate "*" (show . abs <$> 1:as)) term))) == [product (map abs as)]

  describe "expr" $ do
    prop "matches factors" $
      \ a -> run (runNonDet (runCut (parse (show (abs a)) expr))) == [abs a]

    prop "matches multiplication" $
      \ as -> run (runNonDet (runCut (parse (intercalate "*" (show . abs <$> 1:as)) expr))) == [product (map abs as)]

    prop "matches addition" $
      \ as -> run (runNonDet (runCut (parse (intercalate "+" (show . abs <$> 0:as)) expr))) == [sum (map abs as)]

    prop "respects order of operations" . forAll (sized arbNested) $
      \ as -> run (runNonDet (runCut (parse (intercalate "+" (intercalate "*" . map (show . abs) . (1:) <$> [0]:as)) expr))) == [sum (map (product . map abs) as)]

    where arbNested :: Arbitrary a => Int -> Gen [[a]]
          arbNested 0 = pure []
          arbNested n = do
            Positive m <- arbitrary
            let n' = n `div` (m + 1)
            replicateM m (vector n')


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

parens :: (Carrier sig m, Member Symbol sig) => m a -> m a
parens m = char '(' *> m <* char ')'


parse :: (Alternative m, Monad m) => String -> ParseC m a -> m a
parse input = (>>= exhaustive) . flip runParseC input
  where exhaustive ("", a) = pure a
        exhaustive _       = empty

newtype ParseC m a = ParseC { runParseC :: StateC String m a }
  deriving (Applicative, Functor, Monad)

instance (Alternative m, Carrier sig m, Effect sig) => Carrier (Symbol :+: sig) (ParseC m) where
  eff op = ParseC (\ input -> handleSum
    (eff . handleState input runParseC)
    (\ (Satisfy p k) -> case input of
      c:cs | p c -> runParseC (k c) cs
      _          -> empty)
    op)
  {-# INLINE eff #-}


expr :: (Alternative m, Carrier sig m, Member Cut sig, Member Symbol sig) => m Int
expr = do
  i <- term
  call ((i +) <$ char '+' <* cut <*> expr
    <|> pure i)

term :: (Alternative m, Carrier sig m, Member Cut sig, Member Symbol sig) => m Int
term = do
  i <- factor
  call ((i *) <$ char '*' <* cut <*> term
    <|> pure i)

factor :: (Alternative m, Carrier sig m, Member Cut sig, Member Symbol sig) => m Int
factor
  =   read <$> some digit
  <|> parens expr
