{-# LANGUAGE TypeApplications #-}
module Bench.Error
( benchmark
) where

import Control.Carrier.Error.Church as Church
import Control.Carrier.Error.Cont as Cont
import Control.Carrier.Error.Either as Either
import Control.Monad (replicateM_)
import Control.Monad.Trans.Except as Except
import Gauge hiding (benchmark)

benchmark :: Gauge.Benchmark
benchmark = bgroup "Error"
  [ bgroup "pure"
    [ bench "Either"        $ whnf (errorLoop :: Int -> Either Int ()) 10000
    , bench "Church.ErrorC" $ whnf (run . Church.runError @Int (pure . Left) (pure . Right) . errorLoop) 10000
    , bench "Cont.ErrorC"   $ whnf (run . Cont.runError @Int (pure . Right) . errorLoop) 10000
    , bench "Either.ErrorC" $ whnf (run . Either.runError @Int . errorLoop) 10000
    , bench "ExceptT"       $ whnf (run . Except.runExceptT @Int . errorLoop) 10000
    ]
  , bgroup "IO"
    [ bench "Church.ErrorC IO" $ whnfAppIO (Church.runError @Int (pure . Left) (pure . Right) . errorLoop) 10000
    , bench "Cont.ErrorC IO"   $ whnfAppIO (Cont.runError @Int (pure . Right) . errorLoop) 10000
    , bench "Either.ErrorC IO" $ whnfAppIO (Either.runError @Int . errorLoop) 10000
    , bench "ExceptT IO"       $ whnfAppIO (Except.runExceptT @Int . errorLoop) 10000
    ]
  ]

errorLoop :: Has (Error Int) sig m => Int -> m ()
errorLoop i = replicateM_ i (throwError i `catchError` pure @_ @Int)
{-# INLINE errorLoop #-}
