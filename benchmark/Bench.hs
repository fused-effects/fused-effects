{-# LANGUAGE FlexibleContexts, TypeApplications, TypeOperators #-}
module Main where

import Control.Effect
import Control.Effect.Void
import Control.Effect.Writer
import Control.Monad (replicateM_)
import Criterion.Main
import Data.Monoid (Sum(..))

main :: IO ()
main = defaultMain
  [ bgroup "WriterC"
    [ bgroup "Eff"
      [ bench "100"       $ whnf (run . execWriter @_ @_ @(Sum Int) . tellLoop) 100
      , bench "100000"    $ whnf (run . execWriter @_ @_ @(Sum Int) . tellLoop) 100000
      , bench "100000000" $ whnf (run . execWriter @_ @_ @(Sum Int) . tellLoop) 100000000
      ]
    , bgroup "standalone"
      [ bench "100"       $ whnf (fst . runVoidC . flip runWriterC (Sum (0 :: Int)) . tellLoop) 100
      , bench "100000"    $ whnf (fst . runVoidC . flip runWriterC (Sum (0 :: Int)) . tellLoop) 100000
      , bench "100000000" $ whnf (fst . runVoidC . flip runWriterC (Sum (0 :: Int)) . tellLoop) 100000000
      ]
    ]
  ]

tellLoop :: (Applicative m, Carrier sig m, Member (Writer (Sum Int)) sig) => Int -> m ()
tellLoop i = replicateM_ i (tell (Sum (1 :: Int)))
