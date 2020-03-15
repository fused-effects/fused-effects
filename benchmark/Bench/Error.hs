{-# LANGUAGE TypeApplications #-}
module Bench.Error
( benchmark
) where

import Control.Carrier.Error.Church as Church
import Control.Carrier.Error.Cont as Cont
import Control.Carrier.Error.CPS as CPS
import Control.Carrier.Error.Either as Either
import Data.Foldable (for_)
import Control.Monad.Trans.Except as Except
import Gauge hiding (benchmark)

benchmark :: Gauge.Benchmark
benchmark = bgroup "Error"
  [ bench "Either" $ whnf (errorLoop :: Int -> Either Int ()) n
  , bgroup "Identity"
    [ bench "Church.ErrorC" $ whnf (run . Church.runError @Int (pure . Left) (pure . Right) . errorLoop) n
    , bench "Cont.ErrorC"   $ whnf (run . Cont.runError @Int (pure . Right) . errorLoop) n
    , bench "CPS.ErrorC"    $ whnf (run . CPS.runError @Int pure . errorLoop) n
    , bench "Either.ErrorC" $ whnf (run . Either.runError @Int . errorLoop) n
    , bench "ExceptT"       $ whnf (run . Except.runExceptT @Int . errorLoop) n
    ]
  , bgroup "IO"
    [ bench "Church.ErrorC IO" $ whnfAppIO (Church.runError @Int (pure . Left) (pure . Right) . errorLoop) n
    , bench "Cont.ErrorC IO"   $ whnfAppIO (Cont.runError @Int (pure . Right) . errorLoop) n
    , bench "CPS.ErrorC IO"    $ whnfAppIO (CPS.runError @Int pure . errorLoop) n
    , bench "Either.ErrorC IO" $ whnfAppIO (Either.runError @Int . errorLoop) n
    , bench "ExceptT IO"       $ whnfAppIO (Except.runExceptT @Int . errorLoop) n
    ]
  ]
  where
  n = 100000

errorLoop :: Has (Error Int) sig m => Int -> m ()
errorLoop i = for_ [1..i] (\ i -> throwError i `catchError` pure @_ @Int)
{-# INLINE errorLoop #-}
