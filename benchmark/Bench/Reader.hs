{-# LANGUAGE TypeApplications #-}

module Bench.Reader
( benchmark
) where

import Control.Carrier.Reader
import Control.Monad

import Gauge hiding (benchmark)

asking :: Has (Reader Char) sig m => Int -> m ()
asking i = replicateM_ i (ask @Char)

locally :: Has (Reader Char) sig m => Int -> m ()
locally i = replicateM_ i (local @Char succ (ask @Char))

benchmark :: Benchmark
benchmark = bgroup "Control.Carrier.Reader"
  [ bgroup "ask"
    [ bench "10" $ whnf (run . runReader 'a' . asking) 10
    , bench "100" $ whnf (run . runReader 'b' . asking) 100
    , bench "1000" $ whnf (run . runReader 'c' . asking) 1000
    ]
  , bgroup "local"
    [ bench "10" $ whnf (run . runReader 'a' . locally) 10
    , bench "100" $ whnf (run . runReader 'b' . locally) 100
    , bench "1000" $ whnf (run . runReader 'c' . locally) 1000
    ]
  ]
