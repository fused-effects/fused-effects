{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Parser
( example
) where

import Control.Algebra
import Control.Carrier.Cut.Church
import Control.Carrier.NonDet.Church
import Control.Carrier.State.Strict
import Control.Monad (replicateM)
import Data.Char
import Data.List (intercalate)
import GHC.Generics (Generic1)
import Test.Tasty
import Test.Tasty.QuickCheck

example :: TestTree
example = testGroup "parser"
  [ testGroup "parse"
    [ testProperty "returns pure values at the end of input" $
      \ a -> run (runNonDetA (parse "" (pure a))) === [a :: Integer]

    , testProperty "fails if input remains" $
      \ c cs a -> run (runNonDetA (parse (c:cs) (pure (a :: Integer)))) === []
    ]

  , testGroup "satisfy"
    [ testProperty "matches with a predicate" $
      \ c f -> run (runNonDetA (parse [c] (satisfy (applyFun f)))) === if applyFun f c then [c] else []

    , testProperty "fails at end of input" $
      \ f -> run (runNonDetA (parse "" (satisfy (applyFun f)))) === []

    , testProperty "fails if input remains" $
      \ c1 c2 f -> run (runNonDetA (parse [c1, c2] (satisfy (applyFun f)))) === []

    , testProperty "consumes input" $
      \ c1 c2 f -> run (runNonDetA (parse [c1, c2] ((,) <$> satisfy (applyFun f) <*> satisfy (applyFun f)))) === if applyFun f c1 && applyFun f c2 then [(c1, c2)] else []
    ]

  , testGroup "factor"
    [ testProperty "matches positive integers" $
      \ a -> run (runCutA (parse (show (abs a)) factor)) === [abs a]

    , testProperty "matches parenthesized expressions" . forAll (sized arbNested) $
      \ as -> run (runCutA (parse ('(' : intercalate "+" (intercalate "*" . map (show . abs) . (1:) <$> [0]:as) ++ ")") factor)) === [sum (map (product . map abs) as)]
    ]

  , testGroup "term"
    [ testProperty "matches factors" $
      \ a -> run (runCutA (parse (show (abs a)) term)) === [abs a]

    , testProperty "matches multiplication" $
      \ as -> run (runCutA (parse (intercalate "*" (show . abs <$> 1:as)) term)) === [product (map abs as)]
    ]

  , testGroup "expr"
    [ testProperty "matches factors" $
      \ a -> run (runCutA (parse (show (abs a)) expr)) === [abs a]

    , testProperty "matches multiplication" $
      \ as -> run (runCutA (parse (intercalate "*" (show . abs <$> 1:as)) expr)) === [product (map abs as)]

    , testProperty "matches addition" $
      \ as -> run (runCutA (parse (intercalate "+" (show . abs <$> 0:as)) expr)) === [sum (map abs as)]

    , testProperty "respects order of operations" . forAll (sized arbNested) $
      \ as -> run (runCutA (parse (intercalate "+" (intercalate "*" . map (show . abs) . (1:) <$> [0]:as)) expr)) === [sum (map (product . map abs) as)]
    ]
  ]

    where arbNested :: Arbitrary a => Int -> Gen [[a]]
          arbNested 0 = pure []
          arbNested n = do
            Positive m <- arbitrary
            let n' = n `div` (m + 1)
            replicateM m (vector n')


data Symbol m k = Satisfy (Char -> Bool) (Char -> m k)
  deriving (Functor, Generic1)

instance Effect Symbol

satisfy :: Has Symbol sig m => (Char -> Bool) -> m Char
satisfy p = send (Satisfy p pure)

char :: Has Symbol sig m => Char -> m Char
char = satisfy . (==)

digit :: Has Symbol sig m => m Char
digit = satisfy isDigit

parens :: Has Symbol sig m => m a -> m a
parens m = char '(' *> m <* char ')'


parse :: (Alternative m, Monad m) => String -> ParseC m a -> m a
parse input = (>>= exhaustive) . runState input . runParseC
  where exhaustive ("", a) = pure a
        exhaustive _       = empty

newtype ParseC m a = ParseC { runParseC :: StateC String m a }
  deriving (Alternative, Applicative, Functor, Monad)

instance (Alternative m, Algebra sig m, CanThread sig ((,) String)) => Algebra (Symbol :+: sig) (ParseC m) where
  alg (L (Satisfy p k)) = do
    input <- ParseC get
    case input of
      c:cs | p c -> ParseC (put cs) *> k c
      _          -> empty
  alg (R other)         = ParseC (handleCoercible other)
  {-# INLINE alg #-}


expr :: (Alternative m, Has Cut sig m, Has Symbol sig m) => m Int
expr = do
  i <- term
  call ((i +) <$ char '+' <* cut <*> expr
    <|> pure i)

term :: (Alternative m, Has Cut sig m, Has Symbol sig m) => m Int
term = do
  i <- factor
  call ((i *) <$ char '*' <* cut <*> term
    <|> pure i)

factor :: (Alternative m, Has Cut sig m, Has Symbol sig m) => m Int
factor
  =   read <$> some digit
  <|> parens expr
