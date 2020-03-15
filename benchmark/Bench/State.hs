{-# LANGUAGE TypeApplications #-}
module Bench.State
( benchmark
) where

import Control.Carrier.State.Church as C.Church
import Control.Carrier.State.Lazy as C.Lazy
import Control.Carrier.State.Strict as C.Strict
import Data.Foldable (for_)
import Gauge hiding (benchmark)

benchmark :: Benchmark
benchmark = bgroup "State"
  [ bgroup "Identity"
    [ bench "Church.StateC" $ whnf (run . C.Church.execState @Int 0 . modLoop) n
    , bench "Lazy.StateC"   $ whnf (run . C.Lazy.execState @Int 0 . modLoop) n
    , bench "Strict.StateC" $ whnf (run . C.Strict.execState @Int 0 . modLoop) n
    ]
  , bgroup "IO"
    [ bench "Church.StateC" $ whnfAppIO (C.Church.execState @Int 0 . modLoop) n
    , bench "Lazy.StateC"   $ whnfAppIO (C.Lazy.execState @Int 0 . modLoop) n
    , bench "Strict.StateC" $ whnfAppIO (C.Strict.execState @Int 0 . modLoop) n
    ]
  ]
  where
  n = 100000

modLoop :: Has (State Int) sig m => Int -> m ()
modLoop i = for_ [1..i] (modify . (+))
{-# INLINE modLoop #-}
