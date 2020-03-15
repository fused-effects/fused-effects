{-# LANGUAGE TypeApplications #-}
module Bench.Reader
( benchmark
) where

import Control.Carrier.Reader
import Control.Monad (replicateM_)
import Control.Monad.Trans.Reader (runReaderT)
import Gauge hiding (benchmark)

benchmark :: Benchmark
benchmark = bgroup "Reader"
  [ bgroup "ask"
    [ bench "(->)"    $ whnf (`asking` 'c') n
    , bgroup "Identity"
      [ bench "ReaderC" $ whnf (run . runReader 'c' . asking) n
      , bench "ReaderT" $ whnf (run . (`runReaderT` 'c') . asking) n
      ]
    ]
  , bgroup "local"
    [ bench "(->)"    $ whnf (`locally` 'c') n
    , bgroup "Identity"
      [ bench "ReaderC" $ whnf (run . runReader 'c' . locally) n
      , bench "ReaderT" $ whnf (run . (`runReaderT` 'c') . locally) n
      ]
    ]
  ]
  where
  n = 100000

asking :: Has (Reader Char) sig m => Int -> m ()
asking i = replicateM_ i (ask @Char)
{-# INLINE asking #-}

locally :: Has (Reader Char) sig m => Int -> m ()
locally i = replicateM_ i (local @Char succ (ask @Char))
{-# INLINE locally #-}
