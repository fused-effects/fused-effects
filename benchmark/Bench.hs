{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Main
( main
) where

import           Control.Algebra
import           Control.Carrier.Interpret
import           Control.Carrier.State.Strict
import           Control.Carrier.Writer.Strict
import           Control.Monad (replicateM_)
import           Data.Monoid (Sum(..))
import           Gauge

import qualified Bench.Reader as Reader
import qualified Bench.NonDet as NonDet

main :: IO ()
main = defaultMain
  [ Reader.benchmark
  , NonDet.benchmark
  , bgroup "WriterC"
    [ bench "100"   $ whnf (run . execWriter @(Sum Int) . tellLoop) 100
    , bench "1000"  $ whnf (run . execWriter @(Sum Int) . tellLoop) 1000
    , bench "10000" $ whnf (run . execWriter @(Sum Int) . tellLoop) 10000
    ]
  ,
    bgroup "Strict StateC"
    [ bench "100"   $ whnf (run . execState @Int 0 . modLoop) 100
    , bench "1000"  $ whnf (run . execState @Int 0 . modLoop) 1000
    , bench "10000" $ whnf (run . execState @Int 0 . modLoop) 10000
    ]
  ,
    bgroup "InterpretC vs InterpretStateC vs StateC"
    [ bgroup "InterpretC"
      [ bench "100"   $ whnf (\n -> run $ execState @Int 0 $ runInterpret (\ _ (sig :: State Int m k) ctx -> case sig of { Get -> gets @Int (<$ ctx) ; Put s -> ctx <$ put s }) $ modLoop n) 100
      , bench "1000"  $ whnf (\n -> run $ execState @Int 0 $ runInterpret (\ _ (sig :: State Int m k) ctx -> case sig of { Get -> gets @Int (<$ ctx) ; Put s -> ctx <$ put s }) $ modLoop n) 1000
      , bench "10000" $ whnf (\n -> run $ execState @Int 0 $ runInterpret (\ _ (sig :: State Int m k) ctx -> case sig of { Get -> gets @Int (<$ ctx) ; Put s -> ctx <$ put s }) $ modLoop n) 10000
      ]
    , bgroup "InterpretStateC"
      [ bench "100"   $ whnf (\n -> fst . run $ runInterpretState (\ _ (sig :: State Int m k) (s :: Int) ctx -> case sig of { Get -> pure (s, s <$ ctx) ; Put s -> pure (s, ctx) }) 0 $ modLoop n) 100
      , bench "1000"  $ whnf (\n -> fst . run $ runInterpretState (\ _ (sig :: State Int m k) (s :: Int) ctx -> case sig of { Get -> pure (s, s <$ ctx) ; Put s -> pure (s, ctx) }) 0 $ modLoop n) 1000
      , bench "10000" $ whnf (\n -> fst . run $ runInterpretState (\ _ (sig :: State Int m k) (s :: Int) ctx -> case sig of { Get -> pure (s, s <$ ctx) ; Put s -> pure (s, ctx) }) 0 $ modLoop n) 10000
      ]
    , bgroup "StateC"
      [ bench "100"   $ whnf (run . execState @Int 0 . modLoop) 100
      , bench "1000"  $ whnf (run . execState @Int 0 . modLoop) 1000
      , bench "10000" $ whnf (run . execState @Int 0 . modLoop) 10000
      ]
    ]
  ]

tellLoop :: Has (Writer (Sum Int)) sig m => Int -> m ()
tellLoop i = replicateM_ i (tell (Sum (1 :: Int)))

modLoop :: Has (State Int) sig m => Int -> m ()
modLoop i = replicateM_ i (modify (+ (1 :: Int)))
