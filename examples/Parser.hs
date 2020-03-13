{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Parser
( example
) where

import           Control.Algebra
import           Control.Carrier.Cut.Church
import           Control.Carrier.NonDet.Church
import           Control.Carrier.State.Strict
import           Control.Monad (replicateM)
import           Data.Char
import           Data.List (intercalate)
import           Hedgehog
import qualified Hedgehog.Function as Fn
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty
import           Test.Tasty.Hedgehog

example :: TestTree
example = testGroup "parser"
  [ testGroup "parse"
    [ testProperty "returns pure values at the end of input" . property $ do
      a <- forAll genFactor
      run (runNonDetA (parse "" (pure a))) === [a]

    , testProperty "fails if input remains" . property $ do
      c  <- forAll Gen.alphaNum
      cs <- forAll (Gen.string (Range.linear 0 10) Gen.alphaNum)
      a  <- forAll genFactor
      run (runNonDetA (parse (c:cs) (pure a))) === []
    ]

  , testGroup "satisfy"
    [ testProperty "matches with a predicate" . property $ do
      c <- forAll Gen.alphaNum
      f <- (. ord) <$> Fn.forAllFn predicate
      run (runNonDetA (parse [c] (satisfy f))) === [c | f c]

    , testProperty "fails at end of input" . property $ do
      f <- (. ord) <$> Fn.forAllFn predicate
      run (runNonDetA (parse "" (satisfy f))) === []

    , testProperty "fails if input remains" . property $ do
      (c1, c2) <- forAll ((,) <$> Gen.alphaNum <*> Gen.alphaNum)
      f <- (. ord) <$> Fn.forAllFn predicate
      run (runNonDetA (parse [c1, c2] (satisfy f))) === []

    , testProperty "consumes input" . property $ do
      c1 <- forAll Gen.alphaNum
      c2 <- forAll Gen.alphaNum
      f <- (. ord) <$> Fn.forAllFn predicate
      run (runNonDetA (parse [c1, c2] ((,) <$> satisfy f <*> satisfy f))) === [(c1, c2) | f c1, f c2]
    ]

  , testGroup "factor"
    [ testProperty "matches positive integers" . property $ do
      a <- forAll genFactor
      run (runCutA (parse (show (abs a)) factor)) === [abs a]

    , testProperty "matches parenthesized expressions" . property $ do
      as <- forAll (Gen.sized (arbNested genFactor))
      run (runCutA (parse ('(' : intercalate "+" (intercalate "*" . map (show . abs) . (1:) <$> [0]:as) ++ ")") factor)) === [sum (map (product . map abs) as)]
    ]

  , testGroup "term"
    [ testProperty "matches factors" . property $ do
      a <- forAll genFactor
      run (runCutA (parse (show (abs a)) term)) === [abs a]

    , testProperty "matches multiplication" . property $ do
      as <- forAll genFactors
      run (runCutA (parse (intercalate "*" (show . abs <$> 1:as)) term)) === [product (map abs as)]
    ]

  , testGroup "expr"
    [ testProperty "matches factors" . property $ do
      a <- forAll genFactor
      run (runCutA (parse (show (abs a)) expr)) === [abs a]

    , testProperty "matches multiplication" . property $ do
      as <- forAll genFactors
      run (runCutA (parse (intercalate "*" (show . abs <$> 1:as)) expr)) === [product (map abs as)]

    , testProperty "matches addition" . property $ do
      as <- forAll genFactors
      run (runCutA (parse (intercalate "+" (show . abs <$> 0:as)) expr)) === [sum (map abs as)]

    , testProperty "respects order of operations" . property $ do
      as <- forAll (Gen.sized (arbNested (Gen.integral (Range.linear 0 100))))
      run (runCutA (parse (intercalate "+" (intercalate "*" . map (show . abs) . (1:) <$> [0]:as)) expr)) === [sum (map (product . map abs) as)]
    ]
  ]
  where
  arbNested :: Gen a -> Range.Size -> Gen [[a]]
  arbNested _ 0 = pure []
  arbNested g n = do
    m <- Gen.integral (Range.linear 0 10)
    let n' = n `div` (m + 1)
    replicateM (Range.unSize m) (Gen.list (Range.singleton (Range.unSize n')) g)

  predicate = Fn.fn Gen.bool
  genFactor = Gen.integral (Range.linear 0 100)
  genFactors = Gen.list (Range.linear 0 10) genFactor


data Symbol m k = Satisfy (Char -> Bool) (Char -> m k)
  deriving (Functor)


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

instance (Alternative m, Algebra sig m) => Algebra (Symbol :+: sig) (ParseC m) where
  alg hdl sig ctx = case sig of
    L (Satisfy p k) -> do
      input <- ParseC get
      case input of
        c:cs | p c -> ParseC (put cs) *> hdl (k c <$ ctx)
        _          -> empty
    R other         -> ParseC (alg (runParseC . hdl) (R other) ctx)
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
