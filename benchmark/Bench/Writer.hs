{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
module Bench.Writer
( benchmark
) where

import Control.Carrier.Writer.Strict as C.Strict
import Control.Monad (replicateM_)
#if MIN_VERSION_transformers(0,5,6)
import Control.Monad.Trans.Writer.CPS as T.CPS (execWriterT)
#endif
import Control.Monad.Trans.Writer.Lazy as T.Lazy (execWriterT)
import Control.Monad.Trans.Writer.Strict as T.Strict (execWriterT)
import Data.Monoid (Sum(..))
import Gauge hiding (benchmark)

benchmark :: Gauge.Benchmark
benchmark = bgroup "Writer"
  [ bench "(,) w" $ whnf (fst . (tellLoop :: Int -> (Sum Int, ()))) n
  , bgroup "Identity"
    [ bench "Strict.WriterC" $ whnf (run . C.Strict.execWriter @(Sum Int) . tellLoop) n
#if MIN_VERSION_transformers(0,5,6)
    , bench "CPS.WriterT" $ whnf (run . T.CPS.execWriterT @_ @(Sum Int) . tellLoop) n
#endif
    , bench "Lazy.WriterT" $ whnf (run . T.Lazy.execWriterT @_ @(Sum Int) . tellLoop) n
    , bench "Strict.WriterT" $ whnf (run . T.Strict.execWriterT @_ @(Sum Int) . tellLoop) n
    ]
  ]
  where
  n = 1_000_000

tellLoop :: Has (Writer (Sum Int)) sig m => Int -> m ()
tellLoop i = replicateM_ i (tell (Sum (1 :: Int)))
{-# INLINE tellLoop #-}
