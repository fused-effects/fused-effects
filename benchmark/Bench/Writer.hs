{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
module Bench.Writer
( benchmark
) where

import Control.Carrier.Writer.Church as C.Church
import Control.Carrier.Writer.Strict as C.Strict
import Control.Monad (replicateM_)
#if MIN_VERSION_transformers(0,5,6)
import Control.Monad.Trans.Writer.CPS as T.CPS (execWriterT)
#endif
import Control.Monad.Trans.Writer.Lazy as T.Lazy (execWriterT)
import Control.Monad.Trans.Writer.Strict as T.Strict (execWriterT)
import Data.Monoid (Sum(..))
import Test.Tasty.Bench

benchmark :: Benchmark
benchmark = bgroup "Writer"
  [ bench "(,) w" $ whnf (fst . (tellLoop :: Int -> (Sum Int, ()))) n
  , bgroup "Identity"
    [ bench "Church.WriterC" $ whnf (run . C.Church.execWriter @(Sum Int) . tellLoop) n
    , bench "Strict.WriterC" $ whnf (run . C.Strict.execWriter @(Sum Int) . tellLoop) n
#if MIN_VERSION_transformers(0,5,6)
    , bench "CPS.WriterT"    $ whnf (run . T.CPS.execWriterT @_ @(Sum Int) . tellLoop) n
#endif
    , bench "Lazy.WriterT"   $ whnf (run . T.Lazy.execWriterT @_ @(Sum Int) . tellLoop) n
    , bench "Strict.WriterT" $ whnf (run . T.Strict.execWriterT @_ @(Sum Int) . tellLoop) n
    ]
  , bgroup "IO"
    [ bench "Church.WriterC" $ whnfAppIO (C.Church.execWriter @(Sum Int) . tellLoop) n
    , bench "Strict.WriterC" $ whnfAppIO (C.Strict.execWriter @(Sum Int) . tellLoop) n
#if MIN_VERSION_transformers(0,5,6)
    , bench "CPS.WriterT"    $ whnfAppIO (T.CPS.execWriterT @_ @(Sum Int) . tellLoop) n
#endif
    , bench "Lazy.WriterT"   $ whnfAppIO (T.Lazy.execWriterT @_ @(Sum Int) . tellLoop) n
    , bench "Strict.WriterT" $ whnfAppIO (T.Strict.execWriterT @_ @(Sum Int) . tellLoop) n
    ]
  ]
  where
  n = 1000000

tellLoop :: Has (Writer (Sum Int)) sig m => Int -> m ()
tellLoop i = replicateM_ i (tell (Sum (1 :: Int)))
{-# INLINE tellLoop #-}
