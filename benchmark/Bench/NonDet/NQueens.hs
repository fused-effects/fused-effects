{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- Based largely on the implementation by Sreekar Shastry,
-- available at https://github.com/sshastry/queenslogic

module Bench.NonDet.NQueens (benchmark) where

import Control.Applicative
import Control.Monad (guard)
import Data.Foldable
import qualified Data.List as List
import Test.Tasty.Bench hiding (benchmark)

type Square = (Int,Int)
type Board = [Int]

data Diagonal = Row Int
              | Col Int
              | Backslash Int
              | Forwardslash Int
              deriving (Eq, Show)

diags :: Square -> [Diagonal]
diags (i,j) = [ Row i
              , Col j
              , Backslash (j-i)
              , Forwardslash (i+j) ]

isSafeIn :: Square -> Board -> Bool
isSafeIn (i, j) qs = null (diags (i, j) `List.intersect` underThreat)
  where
    qs' = zip [1..length qs] qs
    underThreat = qs' >>= diags

addOne :: (Alternative m, Monad m) => Int -> Board -> m Board
addOne n curr = do
  let i = length curr + 1
  let choose = asum . fmap pure
  j <- choose [1..n]
  guard ((i, j) `isSafeIn` curr)
  pure (curr ++ [j])

queens :: (Alternative m, Monad m) => Int -> m Board
queens n = foldl' (>>=) (pure empty) (replicate n (addOne n))

benchmark :: (Alternative m, Monad m) => String -> (m Board -> [Board]) -> Benchmark
benchmark title runQueens = bgroup title
  [ bench "4"  $ whnf (runQueens . queens) 4
  , bench "8"  $ whnf (runQueens . queens) 8
  , bench "16" $ whnf (runQueens . queens) 16
  ]
{-# INLINE benchmark #-}
