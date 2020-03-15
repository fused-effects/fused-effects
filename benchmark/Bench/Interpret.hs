{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Bench.Interpret
( benchmark
) where

import Control.Carrier.Interpret
import Control.Carrier.State.Strict
import Control.Monad (replicateM_)
import Gauge hiding (benchmark)

benchmark :: Gauge.Benchmark
benchmark = bgroup "Interpret"
  [ bench "InterpretC" $
    whnf (\ n -> run $ execState @Int 0 $ runInterpret (\ _ (sig :: State Int m k) ctx -> case sig of
      Get   -> gets @Int (<$ ctx)
      Put s -> ctx <$ put s) $ modLoop n) n
  , bench "InterpretStateC" $
    whnf (\ n -> fst . run $ runInterpretState (\ _ (sig :: State Int m k) (s :: Int) ctx -> case sig of
      Get   -> pure (s, s <$ ctx)
      Put s -> pure (s, ctx)) 0 $ modLoop n) n
  , bench "StateC" $
    whnf (run . execState @Int 0 . modLoop) n
  ]
  where
  n = 100000

modLoop :: Has (State Int) sig m => Int -> m ()
modLoop i = replicateM_ i (modify (+ (1 :: Int)))
