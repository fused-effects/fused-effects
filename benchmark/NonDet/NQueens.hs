{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses, RankNTypes,
             TypeApplications, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- Based largely on the implementation by Sreekar Shastry,
-- available at https://github.com/sshastry/queenslogic

module NonDet.NQueens (runQueens, benchmark) where

import Control.Applicative
import Control.Effect
import Control.Effect.NonDet
import Control.Monad
import Data.Foldable
import Data.List
import Gauge hiding (benchmark)

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
isSafeIn (i,j) qs = null (diags (i,j) `intersect` underThreat)
  where
    qs' = zip [1..length qs] qs
    underThreat = qs' >>= diags

addOne :: (Member NonDet sig, Carrier sig m, Alternative m) => Int -> Board -> m Board
addOne n curr = do
  let i = length curr + 1
  let choose = asum . fmap pure
  j <- choose [1..n]
  guard ((i, j) `isSafeIn` curr)
  pure (curr ++ [j])

queens :: (Member NonDet sig, Carrier sig m, Alternative m) => Int -> m Board
queens n = foldl' (>>=) (pure empty) (replicate n (addOne n))

runQueens :: Int -> [Board]
runQueens = run . runNonDet . queens

benchmark :: Gauge.Benchmark
benchmark = bgroup "N-queens problem"
  [ bench "4"  $ whnf runQueens 4
  , bench "8"  $ whnf runQueens 8
  , bench "16" $ whnf runQueens 16
  ]

