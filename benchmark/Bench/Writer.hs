{-# LANGUAGE TypeApplications #-}
module Bench.Writer
( benchmark
) where

import Control.Carrier.Writer.Strict as C.Strict
import Control.Monad.Trans.Writer.Lazy as T.Lazy (execWriterT)
import Control.Monad.Trans.Writer.Strict as T.Strict (execWriterT)
import Data.Foldable (for_)
import Data.Monoid (Sum(..))
import Gauge hiding (benchmark)

benchmark :: Gauge.Benchmark
benchmark = bgroup "Writer"
  [ bench "Strict.WriterC" $ whnf (run . C.Strict.execWriter @(Sum Int) . tellLoop) n
  , bench "Lazy.WriterT" $ whnf (run . T.Lazy.execWriterT @_ @(Sum Int) . tellLoop) n
  , bench "Strict.WriterT" $ whnf (run . T.Strict.execWriterT @_ @(Sum Int) . tellLoop) n
  ]
  where
  n = 100000

tellLoop :: Has (Writer (Sum Int)) sig m => Int -> m ()
tellLoop i = for_ [1..i] (tell . Sum)
{-# INLINE tellLoop #-}
