module Bench.State
( benchmark
) where

import Control.Carrier.State.Church as C.Church
import Control.Carrier.State.Lazy as C.Lazy
import Control.Carrier.State.Strict as C.Strict
import Control.Monad.Trans.State.Lazy as T.Lazy (execStateT)
import Control.Monad.Trans.State.Strict as T.Strict (execStateT)
import Data.Foldable (for_)
import Gauge hiding (benchmark)

benchmark :: Benchmark
benchmark = bgroup "State"
  [ bgroup "Identity"
    [ bench "Church.StateC" $ whnf (run . C.Church.execState from . modLoop) n
    , bench "Lazy.StateC"   $ whnf (run . C.Lazy.execState from . modLoop) n
    , bench "Strict.StateC" $ whnf (run . C.Strict.execState from . modLoop) n
    , bench "Lazy.StateT"   $ whnf (run . flip T.Lazy.execStateT from . modLoop) n
    , bench "Strict.StateT" $ whnf (run . flip T.Strict.execStateT from . modLoop) n
    ]
  , bgroup "IO"
    [ bench "Church.StateC" $ whnfAppIO (C.Church.execState from . modLoop) n
    , bench "Lazy.StateC"   $ whnfAppIO (C.Lazy.execState from . modLoop) n
    , bench "Strict.StateC" $ whnfAppIO (C.Strict.execState from . modLoop) n
    , bench "Lazy.StateT"   $ whnfAppIO (flip T.Lazy.execStateT from . modLoop) n
    , bench "Strict.StateT" $ whnfAppIO (flip T.Strict.execStateT from . modLoop) n
    ]
  ]
  where
  from = 0 :: Int
  n = 100000

modLoop :: Has (State Int) sig m => Int -> m ()
modLoop i = for_ [1..i] (modify . (+))
{-# INLINE modLoop #-}
